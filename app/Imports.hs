module Imports (
    module Prelude,
    module App,
    module DB,
    module Lenses,
    module Types,
    module Happstack.Server,
    module HSP, HSPT,
    
    msum, optional, guard, liftIO, forM,
    fromMaybe,
    getCurrentTime,
    Text,

    base
) where

import App
import DB
import Lenses
import Types

import Control.Applicative (optional)
import Control.Monad (msum, guard, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Happstack.Server hiding (redirect)
import HSP
import HSP.Monad (HSPT)
import Prelude hiding (log)

base :: EmbedAsChild (HSPT XML App) p => p -> HTML XML
base content = do
    current_user <- liftHTML $ currentUser >>= maybe (pure Nothing) query
    $(html "base" False)
