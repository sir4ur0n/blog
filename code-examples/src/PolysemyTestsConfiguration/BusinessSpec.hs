module PolysemyTestsConfiguration.BusinessSpec where

import Polysemy

import Test.HUnit

import PolysemyTestsConfiguration.Business
import PolysemyTestsConfiguration.ConfigurationSpec

main :: IO ()
main = do
  _ <- runTestTT $ TestList [test_defaultMinimumAmount_lower, test_minimumAmount_greater]
  pure ()

test_defaultMinimumAmount_lower :: Test
test_defaultMinimumAmount_lower = TestCase $
  let 
    mockLookupEnv _ = Nothing
    result = run . confToMock mockLookupEnv $ myBusinessFunction 400
  in result @?= Left "400 is lower than the minimum allowed amount 500"

test_minimumAmount_greater :: Test
test_minimumAmount_greater = TestCase $
  let 
    mockLookupEnv "MINIMUM_AMOUNT" = Just "250"
    mockLookupEnv _                = Nothing
    result = run . confToMock mockLookupEnv $ myBusinessFunction 400
  in result @?= Right 400
