---
title: Polysemy - Introduction
---

Many Haskellers seem to have a love-hate relationship with `IO`.

Love, because unlike most languages, Haskell programs "tag" functions with side effects using `IO`, leading to programs that are easier to understand, and forcing developers to separate concerns between pure and impure functions.

Hate, because `IO` is binary: either a function has side effects, or it has not, but you don't get much more information from `IO`. 

The thing is, Haskell developers love to carry as much information (and constraints) as possible in types. Everything the compiler checks, we don't have to check them anymore (either through thinking or tests). So we want a finer granularity to identify and separate side effects in the type system. 

Various tactics have emerged through the years to carry more information about those effects: Monad transformers, MTL, `freer-simple`, `fused-effects`, etc.

And right now the new kid in the block is [Polysemy](https://hackage.haskell.org/package/polysemy).

As the readme states, Polysemy requires much less boilerplate and has a zero-cost performance impact.

Let's see how to get started with this new toy!

## Basics
As [explained in an issue](https://github.com/polysemy-research/polysemy/issues/234) I find the first Polysemy example quite complex, and it actually slowed me down in understanding how to make things work. So let's start with a much simpler example!

A Polysemy effect consists of 3 pieces of code:

* The effect declaration
  * This is where you describe the effect, its meaning, and what arguments need to be passed to "use" the effect in your business code
  * Examples of effects:
    * Access the database
    * Log
    * Read configuration
* The effect uses
  * These are the various places in your business code where you need to use the effect
  * Examples of uses:
    * Database
      * Select all users who play Squash
      * Update user "Julien Debon" to change his favorite sport from Volley-ball to Squash
    * Log
      * Log as DEBUG whenever a price modification occurs
      * Log as ERROR when a payment fails
    * Configuration
      * Read the `environment` property (dev / prod) to disable some features
      * Check if the `bypassSecurity` property is set (automation tests)
* The effect interpretation
  * This is where you explain how an effect should be "converted" into a real action. Typically this is where `IO` occurs
  * Examples of interpreters:
    * Convert to a PostgreSQL query and send to the database pool
    * Log to `stdout` or to a log file
    * Read from the environment variables or a property file


