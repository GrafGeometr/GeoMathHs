{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module DB (Id, DB, MonadDB, insert, update, delete, query, makeDBs) where

import Control.Lens (Lens', (&), (^.), (.~), makeLenses)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M (Map, insert, delete, (!?), lookupMax)
import Happstack.Server (FromReqURI)
import Language.Haskell.TH (mkName, Exp(VarE, ConE), DecsQ, Con(RecC), Type(ConT, AppT), Dec(DataD), Name, Info(TyConI), nameBase, reify)
import System.Directory (doesFileExist)

newtype Id a = Id Int deriving newtype (Eq, Ord, Enum, Show, Read, FromReqURI)

newtype DB k v = DB (M.Map k v)

class HasDB a b | -> a where
    path :: FilePath
    db :: Lens' a b

type MonadDB k v a m = (MonadIO m, MonadReader (IORef a) m, HasDB a (DB k v), Show k, Show v, Ord k)

data DBAction k v
    = Update k v
    | Delete k
    deriving (Show, Read)

initDB :: forall v k a. (HasDB a (DB k v), Ord k, Read k, Read v) => IO (DB k v)
initDB = do
    let p = "db/"<>path @a @(DB k v)
    ex <- doesFileExist p
    DB <$> if ex
        then do
            ls <- fmap read . lines <$> readFile p
            return $ foldl (flip \case
                Update k v -> M.insert k v
                Delete k -> M.delete k) mempty ls
        else return mempty

add :: forall a v k. (Show k, Show v, HasDB a (DB k v)) => DBAction k v -> IO ()
add x = appendFile (path @a @(DB k v)) $ show x<>"\n"

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

delete :: forall v k a m. MonadDB k v a m => k -> m ()
delete k = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db @_ @(DB k v)
    add @_ @v $ Delete k
    writeIORef ref $ dbs & db .~ DB (M.delete k values)

query :: forall v k a m. MonadDB k v a m => k -> m (Maybe v)
query k = ask >>= \ref -> liftIO do
    dbs <- readIORef ref
    let DB values = dbs^.db
    return $ values M.!? k

makeDBs :: Name -> DecsQ
makeDBs n = do
    ls <- makeLenses n
    TyConI (DataD [] _ [] Nothing [RecC c fs] _) <- reify n
    is <- concat <$> traverse (\(_f, _, t) -> do
        '_':f <- return $ nameBase _f
        AppT (AppT (ConT dbName) k) v <- return t
        when (dbName /= ''DB) $ fail "Field is not DB"
        [d|
            instance HasDB $(pure $ ConT n) (DB $(pure k) $(pure v)) where
                path = f
                db = $(pure . VarE $ mkName f)
            |]) fs
    (<> is <> ls) <$> [d|
        initDBs :: IO (IORef $(pure $ ConT n))
        initDBs = $(foldr (\_ f -> [| $f <*> initDB |]) [| pure $(pure $ ConE c) |] fs) >>= newIORef
        |]
