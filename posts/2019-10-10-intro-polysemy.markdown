---
title: Polysemy - Part I - Introduction
context: This is part of a series on effect handling in Haskell using Polysemy
---

## IO vs. Effects

Many Haskellers seem to have a love-hate relationship with `IO`.

Love, because unlike most languages, Haskell programs "tag" functions with side effects using `IO`, leading to programs that are easier to understand, and forcing developers to separate concerns between pure and impure functions.

Hate, because `IO` is binary: either a function has side effects, or it has not, but you don't get much more information from `IO`. Whether an `IO` function writes to a file or launches a nuclear missile, you can't tell by reading its type.

The thing is, Haskell developers love to carry as much information (and constraints) as possible in types. Everything the compiler checks, we don't have to check them anymore (either through thinking or tests). So we want a finer granularity to identify and separate side effects in the type system. 

Various tactics have emerged through the years to carry more information about those effects: Monad transformers, MTL, `freer-simple`, `fused-effects`, etc.

And right now the new kid on the block is [Polysemy](https://hackage.haskell.org/package/polysemy).

As the readme states, Polysemy requires much less boilerplate and has a zero-cost performance impact than other solutions. An additional benefit I love - surprisingly it is not mentioned in the readme - is it becomes a lot easier to test side effects!

Let's see how to get started with this new toy!

## Polysemy basics
As [I discussed in an issue](https://github.com/polysemy-research/polysemy/issues/234) I find the first Polysemy example quite complex and lacking explanations, which slowed me down in understanding how to make things work. I feel like code examples have more impact after a high level explanation of concepts. So here we go!

A Polysemy effect consists of 3 pieces of code:

### 1 - Effect declaration

This is where you describe the effect, its meaning, and what arguments need to be passed to "use" the effect in your business code.

Examples:

* Access the database
* Log
* Read configuration

### 2 - Effect uses

These are the various places in your business code where you need to use the effect.

Examples:

* Database
  * Select all users who play Squash
  * Update user "Julien Debon" to change his favorite sport from Volley-ball to Squash
* Log
  * Log as DEBUG whenever a price modification occurs
  * Log as ERROR when a payment fails
* Configuration
  * Read the `environment` property (dev / prod) to disable some features
  * Check if the `bypassSecurity` property is set (automation tests)
  
### 3 - Effect interpretation

This is where you explain how an effect should be "converted" into a real action. Typically this is where `IO` occurs.

Examples:

* Convert to a PostgreSQL query and send to the database pool
* Log to `stdout` or to a log file
* Read from the environment variables or a property file

Now that we better grasp the idea of Polysemy effects, let's start writing code! Head to [my next blog post](2019-10-11-polysemy-first-example.html) for a first, simple example.
