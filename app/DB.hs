{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}

module DB (Id, DB, OrderedDB, MonadDB, MonadOrderedDB, insert, update, delete, query, makeDBs) where

import Lenses

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M (Map, insert, delete, (!?), lookupMax)
import Happstack.Server (FromReqURI)
import Language.Haskell.TH (mkName, Exp(VarE, ConE), DecsQ, Con(RecC), Type(ConT, AppT), Dec(DataD), Name, Info(TyConI), nameBase, reify)
import Prelude
import System.Directory (doesFileExist)
import System.IO (readFile')

newtype Id a = Id Int deriving newtype (Eq, Ord, Enum, Show, Read, FromReqURI, FromJSON)

newtype DB k v = DB (M.Map k v)

newtype OrderedDB k v = OrderedDB [v]

class HasDB a db k v | -> a, k -> v db where
    path :: FilePath
    db :: Lens' a (db k v)

instance (Ord k, Read k, Read v) => IsDB (DBAction k v) (DB k v) where
    dbFromList = DB . foldl (flip \case
        Update k v -> M.insert k v
        Delete k -> M.delete k) mempty

instance Read v => IsDB v (OrderedDB k v) where
    dbFromList = OrderedDB

class Read a => IsDB a db | db -> a where
    dbFromList :: [a] -> db

class (MonadIO m, MonadReader (IORef a) m, HasDB a DB k v, Show k, Show v, Ord k) => MonadDB k v a m | k -> v

type MonadOrderedDB k v a m = (MonadIO m, MonadReader (IORef a) m, HasDB a OrderedDB k v, Show v)

data DBAction k v
    = Update k v
    | Delete k
    deriving (Show, Read)

initDB :: forall a db k v b. (HasDB a db k v, IsDB b (db k v)) => IO (db k v)
initDB = do
    let p = "db/"<>path @a @db @k
    ex <- doesFileExist p
    dbFromList <$> if ex
        then fmap read . lines <$> readFile' p
        else return []

add :: forall a k v. (Show k, Show v, HasDB a DB k v) => DBAction k v -> IO ()
add x = appendFile ("db/"<>path @a @DB @k) $ show x<>"\n"

insert :: MonadDB (Id v) v a m => v -> m (Id v)
insert v = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db
        k = maybe (Id 1) (succ . fst) $ M.lookupMax values
    add $ Update k v
    writeIORef ref $ dbs & db .~ DB (M.insert k v values)
    return k

update :: MonadDB k v a m => k -> v -> m ()
update k v = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db
    add $ Update k v
    writeIORef ref $ dbs & db .~ DB (M.insert k v values)

delete :: MonadDB k v a m => k -> m ()
delete k = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db
    add $ Delete k
    writeIORef ref $ dbs & db .~ DB (M.delete k values)

query :: MonadDB k v a m => k -> m (Maybe v)
query k = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db
    return $ values M.!? k

makeDBs :: Name -> DecsQ
makeDBs n = do
    ls <- makeLenses n
    TyConI (DataD [] _ [] Nothing [RecC c fs] _) <- reify n
    is <- concat <$> traverse (\(f, _, t) -> do
        let f' = nameBase f
        AppT (AppT d k) v <- return t
        (<>) <$> [d|
            instance HasDB $(pure $ ConT n) $(pure d) $(pure k) $(pure v) where
                path = f'
                db = $(pure . VarE . mkName $ '_' : f')
            |] <*> if d == ConT ''DB then [d|
            instance (MonadIO m, MonadReader (IORef $(pure $ ConT n)) m) => MonadDB $(pure k) $(pure v) $(pure $ ConT n) m
            |] else mempty) fs
    (<> is <> ls) <$> [d|
        initDBs :: IO (IORef $(pure $ ConT n))
        initDBs = $(foldr (\_ f -> [| $f <*> initDB |]) [| pure $(pure $ ConE c) |] fs) >>= newIORef
        |]
