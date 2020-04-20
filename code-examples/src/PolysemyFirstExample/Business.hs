module PolysemyFirstExample.Business where

import Polysemy

import PolysemyFirstExample.Logging

myBusinessFunction :: Member Log r => Integer -> Integer -> Sem r Integer
myBusinessFunction m n = do
  logInfo $ "myBusinessFunction was called with parameters " <> show m <> 
            " and " <> show n
  let result = m + n
  logInfo $ "myBusinessFunction result is " <> show result
  return result
