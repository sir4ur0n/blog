module PolysemyFullExample.User where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics

data User = User
  { name :: String,
    age :: Int,
    email :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow User {name, age, email} = toRow (name, age, email)