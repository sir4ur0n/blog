---
title: Polysemy - Part II - First example
context: This is part of a series on effect handling in Haskell using Polysemy
---

## Setup
Let's setup our project as documented in [Polysemy readme](https://hackage.haskell.org/package/polysemy) beforehand (instructions are for Stack projects but I'm sure you will easily find the Cabal/Nix equivalent):
* Add `polysemy` and `polysemy-plugin` to our `package.yaml` `dependencies`
* Add the following to `ghc-options`:
```yaml
- -fplugin=Polysemy.Plugin
- -flate-specialise
- -fspecialise-aggressively
``` 
* Add the following to `default-extensions` (we also add `TemplateHaskell` because we use it to reduce boilerplate):
```yaml
- DataKinds
- FlexibleContexts
- GADTs
- LambdaCase
- PolyKinds
- RankNTypes
- ScopedTypeVariables
- TemplateHaskell
- TypeApplications
- TypeOperators
- TypeFamilies
```

## Logging
A common need in any application is logging. Whether it's technical logs to keep track of batch start/end (and result), audit logs about who did an admin action, or functional logs about a particular feature being used, it's useful to find what happened in our beloved applications.

But logging is an effect! No matter if we send logs to standard output, a log file, or over the network, it has an effect on the world, other than mere processing.

Of course we could use a good ol' `IO` and call it a day, but as explained in the previous post, `IO` is too coarse, we want more granularity. Instead, let's create and use a `Log` effect!

### Effect declaration

Let's cut to the chase:

```haskell
import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log
```

This is pretty dense already, let's analyze bit by bit what's going on!

`data Log m a` is our effect. This is the type that will appear in all our function signatures.
* `Log` is the effect name. Better pick a name that's descriptive of the effect!
* `m` must always be there (you can guess the `m` stands for `Monad` but you don't really need to know what it's used for)
* `()` is the return type of the action. A logging action returns nothing, so we stick to Unit (`()`)

`LogInfo :: String -> Log m ()` is a possible "action" that has the `Log` effect. As you may have guessed, it is an action that takes a `String` to log, and will log it! Note we can have several actions under the same effect, but let's start with one.

`makeSem ''Log` uses Template Haskell to create the `logInfo` function (same name as the action, but with the first letter changed to lowercase). We do not _technically_ need this, but it saves us writing uninteresting boilerplate, so let's stick with it.

In case you are curious, let's check the type of this `logInfo` function:
```
> :t logInfo

logInfo :: (IfStuck (IndexOf r (Found r Log)) (IfStuck r (TypeError ...) (Pure (TypeError ...))) NoErrorFcf, Find r Log, IndexOf r (Found r Log) ~ Log) => String -> Sem r ()
```

You know what? Let's pretend we never saw that. We don't actually need to understand this to use it.

And that's it! We have declared our logging effect. Remember, with effects, we split effect declaration and effect interpretation. This piece of code in no way explains **how** one should log. That is the whole point!

### Effect use
Now that we have created our logging effect, let's use it in our business code! Let's say we currently have this piece of code:

```haskell
myBusinessFunction :: Integer -> Integer -> IO Integer
myBusinessFunction m n = do
  putStrLn $ "myBusinessFunction was called with parameters " <> show m <> " and " <> show n
  let result = m + n
  putStrLn $ "myBusinessFunction result is " <> show result
  return result
```

`IO` is too coarse, we want to replace its use with our shiny new effect. Fear not, my friend, this is as simple as:

```haskell
myBusinessFunction :: Member Log r => Integer -> Integer -> Sem r Integer
myBusinessFunction m n = do
  logInfo $ "myBusinessFunction was called with parameters " <> show m <> " and " <> show n
  let result = m + n
  logInfo $ "myBusinessFunction result is " <> show result
  return result
```

The main changes are:
* the constraint `Member Log r` which tells that `r` must have **at least** the `Log` effect (because we will use it in our implementation)
* the return type `Sem r Integer` which you can read as "A Polysemy monad with the list of effects `r` and which returns an `Integer`". And the only thing we know (and we need to know) is that `r` has the `Log` effect. It may very well have a thousand other effects, or none, we don't care in this business code. We declare the **needed** effects, not the **exhaustive list** of effects
* the use of `logInfo` (remember? It was generated thanks to `makeSem ''Log` in the effect declaration) to actually log stuff

This piece of code is much better! Now our business code better expresses its effects in the type signature (it logs, and cannot do anything else!), no longer has hardcoded the implementation (`putStrLn`), and we haven't added any complexity to our code.

Now you might wonder "This is great, but at some point, somebody's gotta do the actual logging with `putStrLn`!".

### Effect interpretation
This is where the real world catches on us. It's time to explain how the `Log` effect must be interpreted in terms of `putStrLn`. Say the previous business code was consumed as such:

```haskell
main :: IO ()
main = do
  m <- readLn :: IO Integer
  n <- readLn :: IO Integer
  result <- myBusinessFunction m n
  putStrLn $ "The business result is " <> show result
```

After the changes we did to `myBusinessFunction` this code no longer compiles, because `myBusinessFunction` works in the `Sem` monad while `main` works in the `IO` monad.

First, let's write a function to interpret the `Log` effect in terms of `IO`:

```haskell
logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret (\(LogInfo stringToLog) -> embed $ putStrLn stringToLog)
```

There's a lot going on! Don't panic, as impressive as it may look the first time, you will soon get used to it.

* the `Member (Embed IO) r` constraint means `r` must have the ability to do `IO`. Ideally we would write `Member IO r` but since `IO` is not a Polysemy effect, we need to wrap it as an effect thanks to `Embed`. Note we require the `IO` effect because we want our main application to log using `putStrLn`. In our tests, we will write another interpreter with a pure function, thus we will not need to require the `IO` effect
* the `Sem (Log ': r) a -> Sem r a` type signature can be read as "I take a `Sem` monad which has any effect **and the `Log` effect**, and return the same `Sem` monad without that `Log` effect", effectively meaning we are interpreting (destroying) the `Log` effect. Note, in more recent versions of `Polysemy` (unfortunately not yet available on Stackage), this type signature can be replaced with `InterpreterFor Log r`, which makes the function intention even clearer!
* note that the implementation does not explicitly mention the input argument (called [pointfree style](https://wiki.haskell.org/Pointfree)), this is how interpreters usually look
* `interpret` means what follows will be an interpreter
* since all we know about `r` is that it has the `Log` effect, we need to interpret only its actions (`LogInfo`)
* the `(LogInfo stringToLog)` pattern matching lets us capture the string to log when the action to interpret is `LogInfo`
* `putStrLn stringToLog` is the actual logging, however since its type is `IO ()`, we need to wrap it back into our `Sem` monad, thanks to `embed` 

Again, this usually is the toughest part to grasp. Don't worry if it takes time to sink in. Wash, rinse, repeat.

Now we are able to convert a `Sem` monad with the `Log` effect to a `Sem` monad with the `Embed IO` effect. The last piece of the puzzle we need is to convert a `Sem` monad with `IO` to a good ol' `IO`. Thankfully Polysemy already provides such a function, namely `runM`.

Let's head back to our `main` function and explain to the compiler (and the reader) how one is supposed to interpret those hippie effects back into motherland `IO`:  
```haskell
main :: IO ()
main = do
  m <- readLn :: IO Integer
  n <- readLn :: IO Integer
  result <- runM . logToIO $ myBusinessFunction m n
  putStrLn $ "The business result is " <> show result
```

Well, that was simple.

That's it, our code was successfully migrated from monolithic effect `IO` to fine-grained `Log` effect! The additional noise is negligible and the benefit is already interesting, but the benefits increase tenfold in "real" applications with several effects, several actions per effect, several business functions calling each other, and tests.

In the next post, we will see how to write tests for business functions with Polysemy effects. Stay tuned!

Full code:
```haskell
import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log

main :: IO ()
main = do
  m <- readLn :: IO Integer
  n <- readLn :: IO Integer
  result <- runM . logToIO $ myBusinessFunction m n
  putStrLn $ "The business result is " <> show result

myBusinessFunction :: Member Log r => Integer -> Integer -> Sem r Integer
myBusinessFunction m n = do
  logInfo $ "myBusinessFunction was called with parameters " <> show m <> " and " <> show n
  let result = m + n
  logInfo $ "myBusinessFunction result is " <> show result
  return result

logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret (\(LogInfo stringToLog) -> embed $ putStrLn stringToLog)
```
