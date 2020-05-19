module PolysemyFullExample.Effect.Logging where

import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()
  LogError :: String -> Log m ()

makeSem ''Log

logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret $ \case
  (LogInfo stringToLog) -> embed . putStrLn $ "INFO | " ++ stringToLog
  (LogError stringToLog) -> embed . putStrLn $ "ERROR | " ++ stringToLog
