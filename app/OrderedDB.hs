module OrderedDB (OrderedDB, MonadOrderedDB(..), initOrderedDB, insertOrdered, queryManyOrdered) where

import Control.Lens (makeLenses, (%=), use)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (State)
import System.Directory (doesFileExist)

data OrderedDB a = OrderedDB
    { _path :: FilePath
    , _values :: [a]
    }
makeLenses ''OrderedDB

class (MonadIO m, Show a) => MonadOrderedDB a m where
    stateOrderedDB :: State (OrderedDB a) b -> m b

initOrderedDB :: Read a => String -> IO (OrderedDB a)
initOrderedDB n = do
    let p = "db/"<>n
    ex <- doesFileExist p
    OrderedDB p <$> if ex
        then fmap read . lines <$> readFile p
        else return []

insertOrdered :: forall a m. MonadOrderedDB a m => a -> m ()
insertOrdered x = stateOrderedDB @a (use path) >>= liftIO . flip appendFile (show x<>"\n") >>
    stateOrderedDB (values %= (x:))

queryManyOrdered :: MonadOrderedDB a m => (a -> m (Maybe b)) -> m [b]
queryManyOrdered f = stateOrderedDB (use values) >>= takeWhileM where
    takeWhileM [] = return []
    takeWhileM (x:xs) = f x >>= maybe (return []) \y -> (y:) <$> takeWhileM xs
