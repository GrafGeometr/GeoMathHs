{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module App (
    module Happstack.Server,
    module Types,
    module DB,
    module OrderedDB,
    module HSP, HSPT,
    module Data.Set,
    msum, optional, guard, when, lift, fromMaybe, isNothing,
    Text, Data.Text.length, readMaybe, pack, unpack, splitOn,
    liftIO, getCurrentTime, addUTCTime, nominalDay,
    (%~), (&),

    users, emails,
    App, runApp,
    hsx, page, form, (?=),
    newCookie, currentUser, withUser,
    hashPass, checkPass,
    tryQuery,
) where

import DB
import OrderedDB
import Types

import Control.Lens (makeLenses, Lens', Zoom(..), (%~), (&))
import Control.Applicative (Alternative, optional)
import Control.Monad (MonadPlus(..), msum, guard, mfilter, when)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader, ask)
import Control.Monad.State (State, runState)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Password.Bcrypt (checkPassword, hashPassword, mkPassword, Bcrypt, PasswordCheck(..), PasswordHash)
import Data.String (IsString (..))
import Data.Text (Text, intercalate, length, null, unpack, pack, splitOn)
import Happstack.Server
import HSP
import HSP.Monad (HSPT(..))
import Happstack.Server.HSP.HTML (defaultTemplate)
import Language.Haskell.HSX.QQ (hsx)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing)
import Data.Set (Set, toList, fromList, member, singleton)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)

instance IsString Response where
    fromString = toResponse

data DBs = DBs
    { _users :: DB (Id User) User
    , _emails :: DB Text (Id User)
    , _problems :: DB (Id Problem) Problem
    , _archive :: OrderedDB (UTCTime, Id Problem)
    , _pools :: DB (Id Pool) Pool
    , _accessiblePools :: DB (Id User) [Id Pool]
    , _problemsSource :: DB (Id Problem) (Id Pool)
    }
makeLenses ''DBs

newtype App a = App { unApp :: ReaderT (IORef DBs) (ServerPartT IO) a }
    deriving ( Functor, Alternative, Applicative, Monad
             , MonadPlus, MonadIO, HasRqData, ServerMonad
             , WebMonad Response, FilterMonad Response
             , Happstack, MonadReader (IORef DBs)
             )

instance MonadFail App where
    fail _ = mzero

runApp :: App Response -> IO ()
runApp x = do
    createDirectoryIfMissing True "db"
    dbs <- DBs <$> initDB "users" <*> initDB "emails" <*> initDB "problems"
        <*> initOrderedDB "archive" <*> initDB "pools" <*> initDB "accessiblePools"
        <*> initDB "problemsSource"
    ref <- newIORef dbs
    simpleHTTP nullConf $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
        unApp x `runReaderT` ref

appStateDB :: Lens' DBs a -> State a b -> App b
appStateDB lens s = do
    ref <- ask
    (x, new) <- liftIO $ runState (zoom lens s) <$> readIORef ref
    liftIO $ writeIORef ref new
    return x

instance MonadDB (Id User) User App where
    stateDB = appStateDB users

instance MonadDB Text (Id User) App where
    stateDB = appStateDB emails

instance MonadDB (Id Problem) Problem App where
    stateDB = appStateDB problems

instance MonadOrderedDB (UTCTime, Id Problem) App where
    stateOrderedDB = appStateDB archive

instance MonadDB (Id Pool) Pool App where
    stateDB = appStateDB pools

instance MonadDB (Id User) [Id Pool] App where
    stateDB = appStateDB accessiblePools

instance MonadDB (Id Problem) (Id Pool) App where
    stateDB = appStateDB problemsSource

newCookie :: String -> String -> App ()
newCookie name value = addCookie (MaxAge 30000000) $ mkCookie name value

page :: QuasiQuoter
page = QuasiQuoter
    { quoteExp  = \src -> [|
        unHSPT @XML @App $ toResponse <$> defaultTemplate "GeoMath" () $(quoteExp hsx $ "<%>"<>src<>"</%>")
    |]
    , quotePat  = error "the page QuasiQuoter can only be used on expressions."
    , quoteType = error "the page QuasiQuoter can only be used on expressions."
    , quoteDec  = error "the page QuasiQuoter can only be used on expressions."
    }

form :: [String] -> ([Maybe Text] -> App Response) -> ([Text] -> App String) -> App Response
form fields view process = do
    values <- traverse (fmap (mfilter $ not . Data.Text.null) . optional . lookText') fields
    msum [ method GET >> view values
        , method POST >> maybe mzero process (sequenceA values) >>= flip seeOther (toResponse ())
        , seeOther ("?"<>intercalate "&" (zipWith (\f v -> fromString f<>"="<>v) fields $ catMaybes values)) $ toResponse ()
        ]

(?=) :: Text -> Maybe Text -> [Attr Text Text]
name ?= Just val = [name := val]
_ ?= Nothing = []

hashPass :: Text -> App (PasswordHash Bcrypt)
hashPass = hashPassword . mkPassword

checkPass :: Text -> PasswordHash Bcrypt -> Bool
checkPass pass hash = case checkPassword (mkPassword pass) hash of
    PasswordCheckSuccess -> True
    PasswordCheckFail -> False

currentUser :: App (Maybe (Id User))
currentUser = readMaybe <$> lookCookieValue "userId"

withUser :: (Id User -> User -> App Response) -> App Response
withUser f = msum
    [ do
        Just i <- currentUser
        pass <- lookCookieValue "pass"
        Just User{..} <- query @(Id User) i
        guard $ checkPass (pack pass) passwordHash
        f i User{..}
    , seeOther ("/signin" :: String) $ toResponse ()
    ]

tryQuery :: MonadDB k v App => k -> (v -> App Response) -> App Response
tryQuery k f = query k >>= maybe (notFound $ toResponse ()) f
