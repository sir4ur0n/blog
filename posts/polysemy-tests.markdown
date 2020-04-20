---
title: Polysemy - Part III - Tests
context: This is part of a series on effect handling in Haskell using Polysemy
date: 2020-04-17
---

One of the benefits of writing pure code is that it's so easy to test. You provide input, you get output, you assert on the output, that's it.
But "real world applications" have many functions with effects all over the place. And we also need to test those to ensure quality.

The problem is, how do you test effectful code? As the name indicates, naive tests would have various effects, rendering them "hard" to both write and run.

A solution particularly favored in languages with no clear effect boundary is to use ephemeral "containerized" environments, like Docker containers, to run their PostgreSQL databases, Kafka clusters, etc., during tests. These ephemeral containers lower the pain of testing effectful code, but with limited benefits: they remain slow and they are rather complex to write/maintain.

Another solution, when effectful code is well separated from business logic, is to mock the effects, i.e. to "replace" them with fake logic that suits each test exactly. These mocks usually have a greater LOC cost per test compared to containerized environments, but are extremely fast, and easier to maintain over time.

And guess what? Effect frameworks like Polysemy make it pretty simple to mock effects in tests.

## Mocking effects
All we need to do to mock an effect is to change the **interpreter** layer! The effect declaration and the effect use in business code remain unchanged.

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

Let's replace our silencing interpreter with another one, that records all logs, so that we can check exactly what was logged. We will rely on another pre-existing Polysemy effect, namely `Polysemy.Writer`, which is the Polysemy equivalent of `Writer` or `WriterT`:

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
      logs @?= [ "myBusinessFunction was called with parameters 1 and 2"
               , "myBusinessFunction result is 3" ]
```

So what's going on here?

1. `logToRecord` interprets the `Log` effect in terms of `Writer [String]`, i.e. we record all the logged lines as a list of strings (using `tell` from `Polysemy.Writer` to add logs)
2. We run this `Writer` effect using `runWriter` (this will aggregate all recorded logs thanks to the `Monoid` constraint, using list appending)
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

This first example showed how to change the interpreter to mock/record the effect behavior, but a major part of mocking effects is to return a dummy value instead of executing the effect to retrieve the value (e.g. database access or environment variable).

I think it's interesting to showcase another example where the effect action has a return value other than `()`.

You can find the full code example on [my Github repo](https://github.com/Sir4ur0n/blog/tree/master/code-examples/src/PolysemyTestsLogging).

## Intermediary example: environment variables
Let's consider the use case of enviroment variable access.

Many applications need to read some global configuration, often passed by Kubernetes/Rancher through environment variables or secret files. A database URL, the logging level, a port number, an API key, you name it.

### The effect declaration
```haskell
import Polysemy

data Configuration m a where
  ReadConf :: String -> Configuration m (Maybe String)

makeSem ''Configuration
```

### The effect use in business code
```haskell
import Data.Maybe
import Polysemy

myBusinessFunction :: Member Configuration r => Int -> Sem r (Either String Int)
myBusinessFunction amount = do
  maybeMinimumAmount <- fmap (fmap read) (readConf "MINIMUM_AMOUNT")
  let minimumAmount = fromMaybe 500 maybeMinimumAmount
  pure $ if amount >= minimumAmount
           then Right amount
           else Left $ show amount ++ " is lower than the minimum allowed amount " ++ show minimumAmount
```

This function reads the `MINIMUM_AMOUNT` configuration setting, or uses `500` as default value, then checks that the passed value is greater than or equal to the minimum amount.

Note: the double `fmap` may look weird, this is because we want to convert the resulting `String` to an `Int` but there are 2 layers to map over: `IO` and `Maybe`.

Again, the business code is not concerned with **how** the configuration is retrieved. Is it from an environment variable? A file? A cache? A hardcoded value? Or a combination of those? This decision is up to the interpreter!

### The interpreters
This is an example of interpreter that reads from environment variables:
```haskell
import System.Environment
import Polysemy

confToIO :: Member (Embed IO) r => Sem (Configuration ': r) a -> Sem r a
confToIO = interpret (\(ReadConf envVarName) -> embed $ lookupEnv envVarName)
```

Now let's write a mock interpreter for our tests!

As stated in introduction, mocking means that each test gets to decide the behavior of effects. In this particular case, it means the decision of how to convert the configuration name (e.g. `MINIMUM_AMOUNT`) to a value (of type `Maybe String`) is up to each test, not to the interpreter.

Said differently, the interpreter should take as argument how to do this conversion.

In a functional language, it means: the interpreter should take as argument the function `String -> Maybe String`, and each test should pass such a function (the mock behavior).

```haskell
confToMock :: (String -> Maybe String) -> Sem (Configuration ': r) a -> Sem r a
confToMock mockLookupEnv = interpret (\(ReadConf envVarName) -> pure $ mockLookupEnv envVarName)
```

And now a couple of unit tests showing how to use it:
```haskell
import Test.HUnit

test_defaultMinimumAmount_lower = TestCase $
  let 
    mockLookupEnv _ = Nothing
    result = run . confToMock mockLookupEnv $ myBusinessFunction 400
  in 
    result @?= Left "400 is lower than the minimum allowed amount 500"

test_minimumAmount_greater = TestCase $
  let 
    mockLookupEnv "MINIMUM_AMOUNT" = Just "250"
    mockLookupEnv _                = Nothing
    result = run . confToMock mockLookupEnv $ myBusinessFunction 400
  in 
    result @?= Right 400
```

As you can see, each test provides the mocking function `mockLookupEnv` which is then injected in the interpreter.

You can find the full code example on [my Github repo](https://github.com/Sir4ur0n/blog/tree/master/code-examples/src/PolysemyTestsConfiguration).

## Conclusion
As shown in this post, testing (by mocking) is nice and inexpensive in Haskell with an effect framework like Polysemy.

The separation between effect declaration, effect use and effect interpretation is a big plus: tests only need to change the interpreter to mock effects, all other things remaining equal.

We have been using this technique for more than 6 months in my team, and we truly enjoy it. Most of us have experience with mocking techniques and frameworks in other languages (e.g. Java, JavaScript) but testing in Haskell with Polysemy is far more enjoyable, and by a long shot.

Remember: all the things shown here are loosely coupled to the testing libraries (HUnit and QuickCheck) and the effect library (Polysemy). You can achieve similar benefits with any testing or effect library that relies on the same separation of effect declaration, effect use and effect interpretation.

Enjoy testing!
