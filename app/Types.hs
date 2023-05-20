module Types where

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
    , problemTags :: [Tag]
    } deriving (Show, Read)
