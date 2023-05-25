{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import Prelude hiding (log)

import DB
import Lenses
import Types

import Control.Applicative (Alternative, optional)
import Control.Concurrent (putMVar)
import Control.Monad (MonadPlus(..), msum, guard, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Data.Aeson (decode, Value, FromJSON, fromJSON, Result (..))
import Data.IORef (IORef)
import Data.List (mapAccumL)
import Data.Map (Map, (!?))
import Data.Password.Bcrypt (checkPassword, hashPassword, mkPassword, Bcrypt, PasswordCheck(..), PasswordHash)
import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text as T (null)
import qualified Data.Text.IO.Utf8 as T (readFile)
import qualified Data.Text.Lazy as L (Text, unpack)
import Data.Time (UTCTime, getCurrentTime)
import Happstack.Server hiding (redirect)
import Happstack.Server.HSP.HTML ()
import Happstack.Server.SURI (ToSURI)
import HSP
import HSP.Monad (HSPT(..))
import Language.Haskell.HSX.QQ (hsx)
import Language.Haskell.TH (ExpQ, Exp (SigE))
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.Directory (createDirectoryIfMissing)
import System.Random (getStdRandom, randomR)
import Text.Read (readMaybe)

data ArchiveByTime

data DBs = DBs
    { users :: DB (Id User) User
    , names :: DB UserName (Id User)
    , emails :: DB Email EmailInfo
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
    fail err = log ("Failed: "<>show err) >> mzero

instance MonadFail HTML where
    fail = lift . lift . fail

instance EmbedAsAttr (HSPT XML App) (Attr a v) => EmbedAsAttr (HSPT XML App) (Attr a (App v)) where
    asAttr (a := v) = liftHTML v >>= asAttr . (a:=)

instance EmbedAsAttr (HSPT XML App) (Attr a v) => EmbedAsAttr (HSPT XML App) (Attr a (Maybe v)) where
    asAttr (a := Just v) = asAttr $ a := v
    asAttr (_ := Nothing) = return []

instance EmbedAsChild (HSPT XML App) (Id a) where
    asChild = asChild . show

decodeJsonBody :: ServerPartT IO (Map String Value)
decodeJsonBody = askRq >>= \rq -> takeRequestBody rq >>=
    maybe (return mempty) \b -> maybe (do
        liftIO $ putMVar (rqBody rq) b
        decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
        return mempty) return . decode $ unBody b

runApp :: App Response -> IO ()
runApp x = do
    createDirectoryIfMissing True "db"
    ref <- initDBs
    simpleHTTP nullConf $ do
        dec <- decodeJsonBody
        unApp x `runReaderT` ref `runReaderT` dec

newCookie :: ToText a => String -> a -> App ()
newCookie name = addCookie (MaxAge 30000000) . mkCookie name . toText

hashPass :: Text -> App (PasswordHash Bcrypt)
hashPass = hashPassword . mkPassword

checkPass :: Text -> PasswordHash Bcrypt -> Bool
checkPass pass hash = case checkPassword (mkPassword pass) hash of
    PasswordCheckSuccess -> True
    PasswordCheckFail -> False

redirect :: ToSURI uri => uri -> App Response
redirect x = seeOther x $ toResponse ()

loginUser :: Id User -> Text -> App ()
loginUser i pass = do
    newCookie "userId" i
    newCookie "pass" pass
    log $ "Logged in: "<>show i

logoutUser :: App ()
logoutUser = do
    expireCookie "userId"
    expireCookie "pass"

currentUser :: App (Maybe (Id User))
currentUser = (>>= readMaybe) <$> optional (lookCookieValue "userId")

withUser :: (Id User -> User -> App Response) -> App Response
withUser f = msum
    [ do
        Just i <- currentUser
        pass <- readCookieValue "pass"
        Just User{..} <- query i
        guard $ checkPass pass userPasswordHash
        f i User{..}
    , redirect "/login"
    ]

generateToken :: App VerificationToken
generateToken = getStdRandom $ \g ->
    let (g', s) = mapAccumL genChar g [1..30::Int] in (VerificationToken $ pack s, g') where
    xs = ['A'..'Z']<>['a'..'z']<>['0'..'9']
    genChar g _ = let (n, g') = randomR (0, length xs-1) g in (g', xs!!n)

createToken :: Email -> EmailInfo -> App ()
createToken email info = do
    token <- generateToken
    log $ "Generated token for "<>show email<>show token
    update email $ info & _emailVerificationToken ?~ token

tryQuery :: MonadDB k v DBs App => k -> (v -> App Response) -> App Response
tryQuery k f = query k >>= maybe (do
    log $ "Query failed: "<>show k
    notFound $ toResponse "Given ID was not found") f

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
    let src' = foldr (uncurry replace . bimap pack pack) src
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
    SigE <$> quoteExp hsx (unpack if b then pack "<%>"<>src'<>pack "</%>" else src') <*> [t| XMLGenT (HSPT XML App) _ |]

template :: String -> ExpQ
template p = [| runHTML $ base $(html p True) |]

form :: App (Maybe String) -> App Response -> App Response
form post get = msum
    [ method POST >> msum [post, return Nothing] >>= maybe (log "invalid post data" >> get) redirect
    , method GET >> get
    ]

class ToText a where
    toText :: a -> String

instance {-# OVERLAPPING #-} ToText Text where
    toText = unpack

instance {-# OVERLAPPING #-} ToText L.Text where
    toText = L.unpack

instance {-# OVERLAPPING #-} ToText String where
    toText = id

deriving instance {-# OVERLAPPING #-} ToText Tag
deriving instance {-# OVERLAPPING #-} ToText UserName
deriving instance {-# OVERLAPPING #-} ToText Email
deriving instance {-# OVERLAPPING #-} ToText VerificationToken

instance Show a => ToText a where
    toText = show

json :: FromJSON a => String -> App a
json n = App (lift ask) >>= \d -> case fromJSON <$> d !? n of
    Just (Success x) -> return x
    _ -> mzero

arg :: FromReqURI a => String -> App a
arg = lookRead

argStr :: String -> App String
argStr = look >=> \s -> if null s then mzero else return s

argText :: String -> App Text
argText = lookText' >=> \s -> if T.null s then mzero else return s

log :: String -> App ()
log s = liftIO do
    now <- getCurrentTime
    appendFile "log.txt" (show now<>": "<>s<>"\n")
