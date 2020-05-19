module PolysemyFullExample.Controller where

import Polysemy
import PolysemyFullExample.Effect.Database
import PolysemyFullExample.Effect.Logging
import PolysemyFullExample.User
import Servant

type UserAPI = "users" :> (GetUsers :<|> PostUser)

type GetUsers = Get '[JSON] [User]

type PostUser = ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent

myServer :: Members [Database, Log, Embed Handler] r => ServerT UserAPI (Sem r)
myServer = handleGetUsers :<|> handlePostUser

userAPI :: Proxy UserAPI
userAPI = Proxy

-- | This handler only has 1 effect, so we use the 'Member' syntax
handleGetUsers :: Member Database r => Sem r [User]
handleGetUsers = loadUsers

-- | This handler has 3 effects, so we use the 'Members' syntax.
--
-- * 'Database' gives access to 'insertUser' action
-- * 'Log' gives access to 'logInfo' and 'logError' actions
-- * 'Embed Handler' gives access to 'throwError', allowing us to change the HTTP response
handlePostUser :: Members [Database, Log, Embed Handler] r => User -> Sem r NoContent
handlePostUser user =
  if age user >= 18
    then do
      logInfo $ "Inserting user " <> show user <> " in database"
      insertUser user
      pure NoContent
    else do
      logError $ "User " <> show user <> " is a minor, refusing to insert in database"
      embed $ throwError err400 {errBody = "User is a minor"}