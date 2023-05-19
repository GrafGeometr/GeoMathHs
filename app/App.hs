{-# OPTIONS_GHC -Wno-orphans #-}
module App (
    module Happstack.Server,
    module Types,
    module DB,
    module HSP,
    msum, optional, guard, when, fromMaybe, isNothing,
    Data.Text.length, unpack,
    
    users, emails,
    App, runApp,
    page, form, (?=),
    newCookie, currentUser, withUser,
    hashPass, checkPass,
) where

import DB
import Types

import Control.Lens (makeLenses, Lens', Zoom(..))
import Control.Applicative (Alternative, optional)
import Control.Monad (MonadPlus(..), msum, guard, mfilter, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader, ask)
import Control.Monad.State (State, runState)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Password.Bcrypt (checkPassword, hashPassword, mkPassword, Bcrypt, PasswordCheck(..), PasswordHash)
import Data.String (IsString (..))
import Data.Text (Text, intercalate, length, null, unpack, pack)
import Happstack.Server
import HSP
import HSP.Monad (HSPT(..))
import Happstack.Server.HSP.HTML (defaultTemplate)
import Language.Haskell.HSX.QQ (hsx)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Read (readMaybe)

instance IsString Response where
    fromString = toResponse

data DBs = DBs
    { _users :: DB (Id User) User
    , _emails :: DB Text (Id User)
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
    dbs <- DBs <$> initDB "users" <*> initDB "emails"
    ref <- newIORef dbs
    simpleHTTP nullConf $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
        unApp x `runReaderT` ref

appStateDB :: Lens' DBs (DB k v) -> State (DB k v) a -> App a
appStateDB lens s = do
    ref <- ask
    (x, new) <- liftIO $ runState (zoom lens s) <$> readIORef ref
    liftIO $ writeIORef ref new
    return x

instance MonadDB (Id User) User App where
    stateDB = appStateDB users

instance MonadDB Text (Id User) App where
    stateDB = appStateDB emails

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

withUser :: (User -> App Response) -> App Response
withUser f = msum
    [ do
        Just i <- currentUser
        pass <- lookCookieValue "pass"
        Just User{..} <- query @(Id User) i
        guard $ checkPass (pack pass) passwordHash
        f User{..}
    , seeOther ("/signin" :: String) $ toResponse ()
    ]
