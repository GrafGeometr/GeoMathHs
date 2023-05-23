{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module App (
    module DB,
    module Types,
    module HSP, HSPT,
    module Happstack.Server,
    msum,

    App, runApp,
    newCookie, hashPass, currentUser, withUser,
    tryQuery, checkUnique,
    HTML, liftHTML, unHTML, runHTML, html, template,
    ToText(..),
    json,
) where

import DB
import Types

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(..), msum, guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Data.Aeson (decode, Value, FromJSON, fromJSON, Result (..))
import Data.IORef (IORef)
import Data.Map (Map, (!?))
import Data.Maybe (fromMaybe)
import Data.Password.Bcrypt (checkPassword, hashPassword, mkPassword, Bcrypt, PasswordCheck(..), PasswordHash)
import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text.IO.Utf8 as T (readFile)
import qualified Data.Text.Lazy as L (Text)
import Data.Time (UTCTime)
import Happstack.Server
import Happstack.Server.HSP.HTML ()
import HSP
import HSP.Monad (HSPT(..))
import Language.Haskell.HSX.QQ (hsx)
import Language.Haskell.TH (ExpQ, Exp (SigE))
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)

data ArchiveByTime

data DBs = DBs
    { users :: DB (Id User) User
    , emails :: DB Text (Id User)
    , problems :: DB (Id Problem) Problem
    , archive :: OrderedDB ArchiveByTime (UTCTime, Id Problem)
    , pools :: DB (Id Pool) Pool
    }
makeDBs ''DBs

newtype App a = App { unApp :: ReaderT (IORef DBs) (ReaderT (Map String Value) (ServerPartT IO)) a }
    deriving ( Functor, Alternative, Applicative, Monad
             , MonadPlus, MonadIO, HasRqData, ServerMonad
             , WebMonad Response, FilterMonad Response
             , Happstack, MonadReader (IORef DBs)
             )

instance MonadFail App where
    fail _ = mzero

decodeJsonBody :: ServerPartT IO (Map String Value)
decodeJsonBody = askRq >>= fmap (\b -> fromMaybe mempty $ b >>= decode . unBody) . takeRequestBody

runApp :: App Response -> IO ()
runApp x = do
    createDirectoryIfMissing True "db"
    ref <- initDBs
    simpleHTTP nullConf $ do
        dec <- decodeJsonBody
        unApp x `runReaderT` ref `runReaderT` dec

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
        guard $ checkPass (pack pass) userPasswordHash
        f i User{..}
    , seeOther ("/signin" :: String) $ toResponse @Text "Please sign in"
    ]

tryQuery :: MonadDB k v DBs App => k -> (v -> App Response) -> App Response
tryQuery k f = query k >>= maybe (notFound $ toResponse @Text "Given ID was not found") f

checkUnique :: forall v k. MonadDB k v DBs App => k -> App Response -> App Response
checkUnique k x = query k >>= maybe x undefined

instance Monad m => EmbedAsAttr (HSPT XML m) (Attr L.Text String) where
    asAttr (n := x) = asAttr $ n := pack x

type HTML = XMLGenT (HSPT XML App)

liftHTML :: App a -> HTML a
liftHTML = XMLGenT . HSPT

unHTML :: HTML a -> App a
unHTML = unHSPT . unXMLGenT

runHTML :: HTML XML -> App Response
runHTML = fmap toResponse . unHTML

html :: String -> Bool -> ExpQ
html rel b = do
    p <- makeRelativeToProject $ "templates/"<>rel<>".html"
    addDependentFile p
    src <- T.readFile p
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
template p = [| runHTML $ base $(html p True) |]

class ToText a where
    toText :: a -> String

instance ToText Text where
    toText = unpack

instance ToText (Id a) where
    toText = show

json :: FromJSON a => String -> App a
json n = App (lift ask) >>= \d -> case fromJSON <$> d !? n of
    Just (Success x) -> return x
    _ -> mzero
