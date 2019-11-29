---
title: Polysemy - Part I - Introduction
context: This is part of a series on effect handling in Haskell using Polysemy
---

## Effect vs Side effect
Before I took on functional programming, I never made a difference between "effect" and "side effect". Interestingly, there are many [resources](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) on side effects but few on effects. So let's start with a few definitions:

* A function is a piece of software requiring 0, 1 or several parameters, and returning a value.
* A function has an effect when it has an observable interaction with the world outside of that function. Examples of effects are I/O (disk, network, keyboard, screen...), global variable or parameter mutation, program interruption, etc.
* A function has a side effect when it has an effect that was not described by the function - and thus, the caller of the function is unaware the program now has this effect!

Example of function having a side effect:
```java
Integer foo(String input) {
    System.out.println("I am inside foo");
    return 42;
}
```
The caller is well aware this function takes a `String` as input and returns an `Integer`, but it has no idea the function will print something to the standard output without looking at its implementation. Requiring every developer to read through the implementation of each function, and each sub-function called by each function, and so on, obviously doesn't scale to large programs. This leads to buggy, brittle programs that everybody fears maintaining.

Example of function having an effect which is not a side effect:
```java
Integer foo(String input) throws CannotDoThatException {
    // ...
}
```
This function has an effect (program interruption through `CannotDoThatException`) which is definitely not hidden. The author knows very well this function can abruptly stop (and why) without looking at its implementation. This is much safer, and scales much better to programs.

Effects are awesome. A program is merely something that converts electricity into effects, be it a calculation result, a character displayed on your screen or your voice being recorded by a microphone.

I challenge you to write a useful program without effect.

Side effects, on the other hand, are evil. They creep throughout programs, the author or reader unaware of them, and may or may not have unintended consequences whenever they run.

And suddenly one can see why so many developers (myself included) confuse effects with side effects: most programming languages confuse them, too! Checked exceptions aside, Java treats all effects as side effects. Most of us have learnt and practiced unsafe programming for years because all of our effects (useful and intentional!) are side effects (hidden, untraceable, creeping).

And now the good news: some languages fully support and embrace effects, but make side effects impossible, resulting in safer programs for free! As an example, in Haskell, all functions having effects don't return a value of type `Foo`, but of type `IO Foo`, meaning "This function ultimately returns a `Foo` but has effects along the way". And all functions calling it also have to change their type from `Bar` to `IO Bar` to track the propagation of effects.

## IO vs. Effect

Many Haskellers have a love-hate relationship with `IO`.

Love, because unlike most languages, Haskell programs "tag" functions with effects using `IO`, leading to programs that are easier to understand, and forcing developers to separate concerns between pure and impure functions.

Hate, because `IO` is binary: either a function has effects, or it has not, but you do not get much more information from `IO`. Whether an `IO` function writes to a file or launches a nuclear missile, you cannot tell by reading its type.

The thing is, Haskell developers love to carry as much information (and constraints) as possible in types. Everything the compiler checks, we don't have to check them anymore (either through thinking or tests). So we want a finer granularity to identify and separate effects in the type system. 

Various tactics have emerged through the years to carry more information about those effects: Monad transformers, MTL, `freer-simple`, `fused-effects`, etc.

And right now the new kid on the block is [Polysemy](https://hackage.haskell.org/package/polysemy).

As the readme states, Polysemy requires much less boilerplate than other solutions, and has a zero-cost performance (starting with GHC 8.10). An additional benefit I love - surprisingly it is not mentioned in the readme - is it becomes a lot easier to mock effects, and thus test functions with effects!

Let's see how to get started with this new toy.

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

Now that we better grasp the idea of Polysemy effects, let's start writing code! Head to [my next blog post](polysemy-first-example.html) for a first, simple example.
