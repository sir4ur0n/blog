module PolysemyTestsConfiguration.ConfigurationSpec where

import Polysemy

import PolysemyTestsConfiguration.Configuration

confToMock :: (String -> Maybe String) -> Sem (Configuration ': r) a -> Sem r a
confToMock mockLookupEnv = interpret (\(ReadConf envVarName) -> pure $ mockLookupEnv envVarName)
