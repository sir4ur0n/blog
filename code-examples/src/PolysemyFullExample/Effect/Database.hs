module PolysemyFullExample.Effect.Database where

import Database.SQLite.Simple
import PolysemyFullExample.User
import Polysemy

data Database m a where
  MigrateDatabase :: Database m ()
  InsertUser :: User -> Database m ()
  LoadUsers :: Database m [User]

makeSem ''Database

databaseToIO :: Member (Embed IO) r => Connection -> Sem (Database ': r) a -> Sem r a
databaseToIO conn = interpret \case
  MigrateDatabase -> embed do
    execute_ conn "CREATE TABLE user (name TEXT, age INTEGER, email TEXT)"
    insertUser' User {name = "Isaac Newton", age = 372, email = "isaac@newton.co.uk"} conn
    insertUser' User {name = "Albert Einstein", age = 136, email = "ae@mc2.org"} conn
  InsertUser user -> embed $ insertUser' user conn
  LoadUsers -> embed $ loadUsers' conn

insertUser' :: User -> Connection -> IO ()
insertUser' user conn = execute conn "INSERT INTO user (name, age, email) VALUES (?, ?, ?)" user

loadUsers' :: Connection -> IO [User]
loadUsers' conn = query_ conn "SELECT * from user"
