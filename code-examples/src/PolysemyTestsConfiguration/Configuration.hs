module PolysemyTestsConfiguration.Configuration where

import Polysemy
import System.Environment

data Configuration m a where
  ReadConf :: String -> Configuration m (Maybe String)

makeSem ''Configuration

confToIO :: Member (Embed IO) r => Sem (Configuration ': r) a -> Sem r a
confToIO = interpret (\(ReadConf envVarName) -> embed $ lookupEnv envVarName)
