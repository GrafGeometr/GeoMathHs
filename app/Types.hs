module Types where

import DB
import Lenses

import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Password.Bcrypt (Bcrypt, PasswordHash)
import Happstack.Server (FromReqURI)
import HSP (EmbedAsChild, XML)
import HSP.Monad (HSPT)
import Prelude

newtype Tag = Tag { tagName :: Text } deriving newtype (Eq, Ord, Show, Read, FromReqURI, FromJSON, EmbedAsChild (HSPT XML m))

newtype UserName = UserName Text deriving newtype (Eq, Ord, Show, Read, FromReqURI, FromJSON, EmbedAsChild (HSPT XML m))

newtype Email = Email Text deriving newtype (Eq, Ord, Show, Read, FromReqURI, FromJSON, EmbedAsChild (HSPT XML m))

data User = User
    { userName :: UserName
    , userPasswordHash :: PasswordHash Bcrypt
    , userDateCreated :: UTCTime
    , userEmails :: Set Email
    , userPools :: Set (Id Pool)
    } deriving (Show, Read)

newtype VerificationToken = VerificationToken Text deriving newtype (Eq, Ord, Show, Read, FromReqURI, FromJSON, EmbedAsChild (HSPT XML m))

data EmailInfo = EmailInfo
    { emailUser :: Id User
    , emailDateCreated :: UTCTime
    , emailVerificationToken :: Maybe VerificationToken -- Nothing if verified
    } deriving (Show, Read)

data Problem = Problem
    { problemPool :: Maybe (Id Pool)
    , problemCondition :: Text
    , problemSolution :: Text
    , problemTags :: Set Tag
    } deriving (Show, Read)

data Role
    = Owner
    | Participant
    | Invited
    deriving (Show, Read)

data Pool = Pool
    { poolName :: Text
    , poolProblems :: Set (Id Problem)
    , poolMembers :: Map (Id User) Role
    } deriving (Show, Read)

makeAllLenses [''User, ''EmailInfo, ''Problem, ''Pool]
