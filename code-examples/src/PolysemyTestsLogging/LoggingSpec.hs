-- | Test interpreters
module PolysemyTestsLogging.LoggingSpec where

import Polysemy
import Polysemy.Writer

import PolysemyTestsLogging.Logging

logToSilence :: Sem (Log ': r) a -> Sem r a
logToSilence = interpret (\(LogInfo _) -> pure ())

logToRecord :: Member (Writer [String]) r => Sem (Log ': r) a -> Sem r a
logToRecord = interpret (\(LogInfo stringToLog) -> tell [stringToLog])
