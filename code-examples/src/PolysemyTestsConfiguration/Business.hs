module PolysemyTestsConfiguration.Business where

import Data.Maybe
import Polysemy

import PolysemyTestsConfiguration.Configuration

myBusinessFunction :: Member Configuration r => Int -> Sem r (Either String Int)
myBusinessFunction amount = do
  maybeMinimumAmount <- fmap (fmap read) (readConf "MINIMUM_AMOUNT")
  let minimumAmount = fromMaybe 500 maybeMinimumAmount
  pure $ if amount >= minimumAmount
           then Right amount
           else Left $ show amount ++ " is lower than the minimum allowed amount " ++ show minimumAmount
