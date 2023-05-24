module Types where

import DB
import Lenses

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Password.Bcrypt (Bcrypt, PasswordHash)
import Happstack.Server (FromReqURI)

newtype Tag = Tag { tagName :: Text } deriving newtype (Eq, Ord, Show, Read, FromReqURI)

data User = User
    { userName :: Text
    , userPasswordHash :: PasswordHash Bcrypt
    , userDateCreated :: UTCTime
    , userEmails :: Set Text
    , userPools :: Set (Id Pool)
    } deriving (Show, Read)

newtype VerificationToken = VerificationToken Text deriving (Show, Read)

data EmailInfo = EmailInfo
    { emailDateCreated :: UTCTime
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
    | Participiant
    | Invited
    deriving (Show, Read)

data Pool = Pool
    { poolName :: Text
    , poolProblems :: Set (Id Problem)
    , poolMembers :: Map (Id User) Role
    } deriving (Show, Read)

makeAllLenses [''User, ''EmailInfo, ''Problem, ''Pool]
