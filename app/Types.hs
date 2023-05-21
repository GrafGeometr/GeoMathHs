module Types where

import DB

import Data.Set (Set)
import Data.Text (Text)
import Data.Password.Bcrypt (Bcrypt, PasswordHash)
import Happstack.Server (FromReqURI)

newtype Tag = Tag { tagName :: Text } deriving newtype (Eq, Ord, Show, Read, FromReqURI)

data User = User
    { name :: Text
    , email :: Text
    , passwordHash :: PasswordHash Bcrypt
    } deriving (Show, Read)

data Problem = Problem
    { condition :: Text
    , solution :: Text
    , problemTags :: Set Tag
    } deriving (Show, Read)

data Pool = Pool
    { poolProblems :: Set (Id Problem)
    , poolOwner :: Id User
    , poolEditAccess :: Set (Id User)
    } deriving (Show, Read)
