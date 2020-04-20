module PolysemyFirstExample.Logging where

import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log

logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret (\(LogInfo stringToLog) -> embed $ putStrLn stringToLog)
