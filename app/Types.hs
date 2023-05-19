module Types where

import Data.Text (Text)
import Data.Password.Bcrypt (Bcrypt, PasswordHash)

import DB (Id)

data User = User
    { userId :: Id User
    , name :: Text
    , email :: Text
    , passwordHash :: PasswordHash Bcrypt
    } deriving (Show, Read)
