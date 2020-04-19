---
title: Polysemy - Part III - Tests
context: This is part of a series on effect handling in Haskell using Polysemy
date: 2020-104-17
---

Note: This post uses HUnit for unit tests and QuickCheck for property based tests.

## Mocking effects
A common need when writing tests for effectful code is to **mock** the effects. The benefits of mocking are:

* No struggle to build temporary environments, like a test database
* Tests are fast (no environment startup cost)
* Each test focuses on a single function (separation of concern)

Now the good news: it is pretty simple to mock effects using a library like Polysemy. All you need to do is change the **interpreter**!

I will reuse the example from my previous post:

```haskell
import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log

myBusinessFunction :: Member Log r => Integer -> Integer -> Sem r Integer
myBusinessFunction m n = do
  logInfo $ "myBusinessFunction was called with parameters " <> show m <> 
            " and " <> show n
  let result = m + n
  logInfo $ "myBusinessFunction result is " <> show result
  return result

logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret (\(LogInfo stringToLog) -> embed $ putStrLn stringToLog)
```

### Step 1: No mocking
Let's first write a test where the logging still happens in `IO` (logging to `stdout`):

```haskell
import Polysemy
import Test.HUnit

test_1and2is3 = TestCase $ do
  result <- runM . logToIO $ myBusinessFunction 1 2
  result @?= 3
```

The test passes but `stdout` was polluted with the logs:
```
Cases: 1  Tried: 0  Errors: 0  Failures: 0myBusinessFunction was called with parameters 1 and 2
myBusinessFunction result is 3
Cases: 1  Tried: 1  Errors: 0  Failures: 0
```
Imagine when you have hundreds of tests running, your terminal (or your CI logs) will quickly get cluttered.
Even worse: if our logging interpreter logged in a file, each test run would create and write into such a log file!
This would also not scale well with other effects, like database calls.

We can do better.

### Step 2: Disable logs

Our goal is to write tests for `myBusinessFunction` without actually logging to `stdout`. 

Let's write another interpreter `logToSilence` for the `Log` effect, except this interpreter will simply ignore the log and do nothing:

```haskell
logToSilence :: Sem (Log ': r) a -> Sem r a
logToSilence = interpret (\(LogInfo stringToLog) -> pure ())

test_1and2is3 = TestCase $ do
  result <- runM . logToSilence $ myBusinessFunction 1 2
  result @?= 3
```

Now the logs are clean, the effect was nicely interpreted:
```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
```

Note, we previously **had to** run in `IO` because `logToIO` required it. Our silencing interpreter is pure, though, so we can replace Polysemy's `runM` with `run` and work with pure code:

```haskell
test_1and2is3 = TestCase $
  let result = run . logToSilence $ myBusinessFunction 1 2
  in  result @?= 3
```

### Step 3: Test effects too
Rather than silencing those logs, maybe logging is part of our requirements. In such case, we should actually check that the function logs correctly!

Let's replace our silencing interpreter with another one, that records all logs, so that we can check exactly what was logged. We will rely on another pre-existing Polysemy effect, namely `Polysemy.State`, which is the Polysemy equivalent of `State` or `StateT`:

```haskell
import Polysemy
import Polysemy.Writer

import Test.HUnit

logToRecord :: Member (Writer [String]) r => Sem (Log ': r) a -> Sem r a
logToRecord = interpret (\(LogInfo stringToLog) -> tell [stringToLog])

test_1and2is3 = TestCase $
  let (logs, result) = run . runWriter . logToRecord $ myBusinessFunction 1 2
  in do
      result @?= 3
      logs @?= ["myBusinessFunction was called with parameters 1 and 2", "myBusinessFunction result is 3"]
```

So what's going on here?

1. `logToRecord` interprets the `Log` effect in terms of `Writer [String]`, i.e. we record all the logged lines as a list of strings
2. We run this `Writer` effect using `runState`
3. We can now assert both on the business result `3` and on the logged lines

That's it! We have successfully removed the `IO` effect, our tests are pure, yet we can fully assert on both the business results and the effects!

Note: Technically we should use `runWriterAssocR` instead of `runWriter` since the monoid is a list, for performance reasons, but this is beyond the scope of this post.

It is exactly the same for property based tests (PBT). Say we want to (quick)check our business function is associative (on the result) and check that the last log will be the same (others will not because of order of application):

```haskell
import Polysemy
import Polysemy.Writer

import Test.QuickCheck

logToRecord :: Member (Writer [String]) r => Sem (Log ': r) a -> Sem r a
logToRecord = interpret (\(LogInfo stringToLog) -> tell [stringToLog])

test_associative = \a b c ->
  let
    (logsAB_then_C, resultAB_then_C) = run . runWriter . logToRecord $ do
      resultAB <- myBusinessFunction a b
      myBusinessFunction resultAB c
    (logsA_then_BC, resultA_then_BC) = run . runWriter . logToRecord $ do
      resultBC <- myBusinessFunction b c
      myBusinessFunction a resultBC
   in
    last logsAB_then_C == last logsA_then_BC && resultAB_then_C == resultA_then_BC
```
