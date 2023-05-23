{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module App (
    module DB,
    module Types,
    module HSP, HSPT,

    App, runApp,
    newCookie, hashPass, currentUser, withUser,
    tryQuery,
    liftHTML, unHTML, html, template,
    ToText(..),
) where

import DB
import Types

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(..), msum, guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Data.IORef (IORef)
import Data.Password.Bcrypt (checkPassword, hashPassword, mkPassword, Bcrypt, PasswordCheck(..), PasswordHash)
import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text.Lazy as L (Text)
import Data.Time (UTCTime)
import Happstack.Server (FilterMonad, ServerMonad, ServerPartT, WebMonad, Response, Happstack, HasRqData, simpleHTTP, nullConf, decodeBody, defaultBodyPolicy, addCookie, CookieLife (..), mkCookie, lookCookieValue, seeOther, toResponse, notFound)
import HSP
import HSP.Monad (HSPT(..))
import Language.Haskell.HSX.QQ (hsx)
import Language.Haskell.TH (ExpQ, Exp (SigE))
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)

data DBs = DBs
    { _users :: DB (Id User) User
    , _emails :: DB Text (Id User)
    , _problems :: DB (Id Problem) Problem
    , _archive :: OrderedDB (UTCTime, Id Problem)
    , _pools :: DB (Id Pool) Pool
    , _accessiblePools :: DB (Id User) [Id Pool]
    , _problemsSource :: DB (Id Problem) (Id Pool)
    }
makeDBs ''DBs

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
    ref <- initDBs
    simpleHTTP nullConf $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
        unApp x `runReaderT` ref

newCookie :: String -> String -> App ()
newCookie name value = addCookie (MaxAge 30000000) $ mkCookie name value

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
        Just User{..} <- query i
        guard $ checkPass (pack pass) passwordHash
        f i User{..}
    , seeOther ("/signin" :: String) $ toResponse @Text "Please sign in"
    ]

tryQuery :: MonadDB k v DBs App => k -> (v -> App Response) -> App Response
tryQuery k f = query k >>= maybe (notFound $ toResponse @Text "Given ID was not found") f

instance Monad m => EmbedAsAttr (HSPT XML m) (Attr L.Text String) where
    asAttr (n := x) = asAttr $ n := pack x

liftHTML :: App a -> XMLGenT (HSPT XML App) a
liftHTML = XMLGenT . HSPT

unHTML :: XMLGenT (HSPT XML App) a -> App a
unHTML = unHSPT . unXMLGenT

html :: String -> Bool -> ExpQ
html rel b = do
    p <- makeRelativeToProject $ "templates/"<>rel<>".html"
    addDependentFile p
    src <- liftIO $ T.readFile p
    let src' = foldr (uncurry replace) src
            [ ("{%", "<%")
            , ("%}", "%>")
            , ("{/", "<%>")
            , ("/}", "</%>")
            , ("{{", "\"<>toText(")
            , ("}}", ")<>\"")
            , ("=\"", "=(\"")
            , ("= \"", "= (\"")
            , ("\" ", "\") ")
            , ("\">", "\")>")
            , ("\"/>", "\")/>")
            , ("required", "required=True")
            ]
    SigE <$> quoteExp hsx (unpack if b then "<%>"<>src'<>"</%>" else src') <*> [t| XMLGenT (HSPT XML App) _ |]

template :: String -> ExpQ
template p = [| unHTML $ base $(html p True) |]

class ToText a where
    toText :: a -> String

instance ToText Text where
    toText = unpack

instance ToText (Id a) where
    toText = show
