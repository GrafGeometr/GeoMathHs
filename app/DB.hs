{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module DB (Id, DB, MonadDB(..), initDB, insert, update, delete, query) where

import qualified Data.Map as M
import Control.Lens (use, uses, (%=), makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadIO(..), runState, MonadState(state), State, StateT, execState)
import System.Directory (doesFileExist)
import Data.Foldable (traverse_)
import Happstack.Server (FromReqURI)

newtype Id a = Id Int deriving newtype (Eq, Ord, Enum, Show, Read, FromReqURI)

data DB k v = DB
    { _path :: FilePath
    , _values :: M.Map k v
    }
makeLenses ''DB

class (MonadIO m, Ord k, Show k, Show v) => MonadDB k v m where
    stateDB :: State (DB k v) a -> m a

instance (MonadIO m, Ord k, Show k, Show v) => MonadDB k v (StateT (DB k v) m) where
    stateDB = state . runState

data DBAction k v
    = Update k v
    | Delete k
    deriving (Show, Read)

initDB :: (Ord k, Read k, Read v) => String -> IO (DB k v)
initDB n = liftIO do
    let p = "db/"<>n
        db = DB p mempty
    ex <- doesFileExist p
    if ex
        then do
            ls <- fmap read . lines <$> readFile p
            return $ traverse_ (\case
                Update i val -> update' i val
                Delete i -> delete' i) ls `execState` db
        else return db

update' :: Ord k => k -> v -> State (DB k v) ()
update' k v = values %= M.insert k v

delete' :: Ord k => k -> State (DB k v) ()
delete' k = values %= M.delete k

add :: forall k v m. MonadDB k v m => DBAction k v -> m ()
add x = stateDB (use @(DB k v) path) >>= liftIO . flip appendFile (show x<>"\n")

insert :: forall a m. MonadDB (Id a) a m => a -> m (Id a)
insert v = stateDB @_ @a (uses values $ maybe (Id 1) (succ . fst) . M.lookupMax) >>= \k ->
    k <$ update k v

update :: MonadDB k v m => k -> v -> m ()
update k v = do
    add $ Update k v
    stateDB $ update' k v

delete :: forall k v m. MonadDB k v m => k -> m ()
delete k = do
    add @k @v $ Delete k
    stateDB @k @v $ delete' k

query :: MonadDB k v m => k -> m (Maybe v)
query = stateDB . uses values . M.lookup
