module PolysemyFirstExample.Lib where

import Polysemy

import PolysemyFirstExample.Logging
import PolysemyFirstExample.Business

main :: IO ()
main = do
  m <- readLn :: IO Integer
  n <- readLn :: IO Integer
  result <- runM . logToIO $ myBusinessFunction m n
  putStrLn $ "The business result is " <> show result
