-- | This is the top-level application.
--
-- Once started, you can toy around with the HTTP API as such:
--
-- * Load users from (in-memory) database: @curl http://localhost:8081/users@
-- * Insert a valid user: @curl -H 'Content-Type: application/json' http://localhost:8081/users -d '{"name":"John Doe", "age":27, "email":"john@doe.com"}'@
-- * Insert an invalid user: @curl -H 'Content-Type: application/json' http://localhost:8081/users -d '{"name":"John Doe", "age":13, "email":"john@doe.com"}'@
module PolysemyFullExample.Application where

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, close, open)
import qualified Network.Wai.Handler.Warp as Warp
import PolysemyFullExample.Controller
import Polysemy
import PolysemyFullExample.Effect.Database
import PolysemyFullExample.Effect.Logging
import Servant (Handler, hoistServer, serve)

-- | This is where the plumbing happens. Typically here you create your database connection pool, configure your Kafka client, parse configuration from environment variables, etc.
--
-- Note the use of 'hoistServer' which allows us to use any context (in our case, a Polysemy one) as long as we provide a way to convert it back into Servant's 'Handler'
main :: IO ()
main = do
  conn <- open ":memory:"
  runM . databaseToIO conn $ migrateDatabase
  Warp.run 8081 . serve userAPI $ hoistServer userAPI (polysemyToHandler conn) myServer
  close conn

-- | Interpret a list of Polysemy effects in terms of Servant's Handler
--
-- Note that order of interpreters matters!
polysemyToHandler :: Connection -> Sem '[Log, Database, Embed IO, Embed Handler] a -> Handler a
polysemyToHandler conn = runM . ioToHandler . databaseToIO conn . logToIO

-- | Rewrap (lift) IO as Servant's Handler
ioToHandler :: Member (Embed Handler) r => Sem (Embed IO ': r) a -> Sem r a
ioToHandler = interpret \(Embed io) -> embed $ liftIO io