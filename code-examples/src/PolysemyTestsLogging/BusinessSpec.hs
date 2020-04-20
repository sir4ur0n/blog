module PolysemyTestsLogging.BusinessSpec where

import Polysemy
import Polysemy.Writer

import Test.HUnit
import Test.QuickCheck

import PolysemyTestsLogging.Business

import PolysemyTestsLogging.LoggingSpec

main :: IO ()
main = do
  _ <- runTestTT test_1and2is3
  quickCheck test_associative

test_1and2is3 :: Test
test_1and2is3 = TestCase $
  let (logs, result) = run . runWriter . logToRecord $ myBusinessFunction 1 2
  in do
      result @?= 3
      logs @?= ["myBusinessFunction was called with parameters 1 and 2", "myBusinessFunction result is 3"]

test_associative :: Integer -> Integer -> Integer -> Bool
test_associative = \a b c ->
  let
    (logsAB_C, resultAB_C) = run . runWriter . logToRecord $ do
      resultAB <- myBusinessFunction a b
      myBusinessFunction resultAB c
    (logsA_BC, resultA_BC) = run . runWriter . logToRecord $ do
      resultBC <- myBusinessFunction b c
      myBusinessFunction a resultBC
   in
    last logsAB_C == last logsA_BC && resultAB_C == resultA_BC
