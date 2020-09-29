---
title: Adapting a data structure to business needs
context: A beginner-to-intermediary post about reusing and changing your data structures, following closely your business requirements
date: 2020-09-16
---

## The context

This post is about finding the sweet spot between type safety - code that compiles Just Works © - and development/maintenance efficiency on your real world Haskell project.

Say we have some Haskell types modeling our business domain:
```haskell
data User = User {
  firstName :: Text,
  lastName :: Text,
  shoppingBasket :: Map Int Item
}

data Item = Item {
  identifier :: Int
}
```

And then some day we need to provide an HTTP endpoint to expose data to our frontend. However we must also provide the localized name of each item in the shopping basket. We already have a table in database with the localized name for each item. All that's left to do is model this and write a function to load the label from database.

How should we model this?

## Solutions that usually come to mind

### Adapt the existing types
Let's add an innocent new field to `Item` and we are good to go, right? But what type should it be?

#### Add a `Text` field
```haskell
data Item = Item {
  identifier :: Int,
  localizedName :: Text
}
```
The problem here is that it now becomes impossible to construct an `Item` without a localization, while most business needs (e.g. calculating the price, making the payment) don't need any. This will force all existing code to call the database for no reason! Or even worse, developers will use an empty text to reprsent "No localization done", which is brittle/error prone.

#### Add a `Maybe Text` field
```haskell
data Item = Item {
  identifier :: Int,
  localizedName :: Maybe Text
}
```
This is an improvement over the previous solution as the type now expresses the 2 possible scenarios: unlocalized items will have a `Nothing` localized name, while localized items will have a `Just Text`.

But this doesn't follow the "Make invalid states irrepresentable" principle.

Indeed if a function receives an `Item` and needs to access its localized name, it can't express it in the signature:
```haskell
describeItem :: Item -> Text
describeItem Item {identifier, localizedName = Just name} = "[" <> show identifier <> "] " <> name
describeItem _ = error "I wanted a localized Item :'("
```
Of course we can change its signature in turn to `describeItem :: Item -> Maybe Text`, but this is only fixing the problem *downstream*, instead of *upstream*.

The *upstream* problem is that we couldn't express in the signature that we needed a localized `Item`. Any caller that **knows** (has proved) it has a localized `Item` should be able to call us, and all others should not.

Similarly, we are unable to ensure *by compilation* that we will only return localized items to the frontend.

This leads us to another common solution, which solves these problems.

### Duplicate types
While "duplicate" is some kind of taboo word amongst developers, duplicating types is actually a sure way to not mix unlocalized and localized items:
```haskell
data UnlocalizedUser = UnlocalizedUser {
  firstName :: Text,
  lastName :: Text,
  shoppingBasket :: Map Int UnlocalizedItem
}

data UnlocalizedItem = UnlocalizedItem {
  identifier :: Int
}

data LocalizedUser = LocalizedUser {
  firstName :: Text,
  lastName :: Text,
  shoppingBasket :: Map Int LocalizedItem
}

data LocalizedItem = LocalizedItem {
  identifier :: Int,
  name :: Text
}
```
That's it! We have made invalid states irrepresentable (the function above would become `describeItem :: LocalizedItem -> Text`), everything is safe.

But...

To reach this safety, we have sacrificed a lot!

First, code duplication means double burden of maintenance, documentation, etc. Whenever I add or modify a field, I need to do the same change in all duplicated types. And should I forget to change all impacted types, I will risk losing information or introducing bugs.

Second, I have lost the ability to call functions that **do not care if the `Item` is localized or not**.

Consider this function:
```haskell
loadItemImage :: UnlocalizedItem -> IO Image
loadItemImage UnlocalizedItem {identifier} = callImageBackend identifier
```
The function `loadItemImage` does not care if the item is localized or not. All it cares about is having an item - which always has an identifier.
Imagine we have a `LocalizedItem` and want to retrieve its image. We are doomed to:
* either duplicate the `loadItemImage` function to provide one taking a `LocalizedItem`
* or convert our `LocalizedItem` to an `UnlocalizedItem` and then call `loadItemImage`

The maintenance toll is getting big, and so is the developer User Experience in this code base.

## Another solution
Ideally we want a solution that:
* is type safe (invalid states are irrepresentable)
* has no or low boilerplate
* allows to specify in function signatures whether a **localized or unlocalized** `Item` is necessary
* allows to specify in function signatures if an `Item` **localization is irrelevant**
* does not require unnecessary (business) conversions

Without further ado (I will explain the syntax and required extensions later), here's a solution that checks all our needs!

```haskell
data LocalizationStatus = Unlocalized | Localized

data User localizationStatus = User {
  firstName :: Text,
  lastName :: Text,
  shoppingBasket :: Map Int (Item localizationStatus)
}

data Item localizationStatus = Item {
  identifier :: Int,
  name :: ItemName localizationStatus
}

type family ItemName (localizationStatus :: LocalizationStatus)
type instance ItemName 'Unlocalized = ()
type instance ItemName 'Localized = Text
```

# foobar
* Intro
  * 3-level nested data structure: User -> ShoppingBasket -> Product
  * function to transform following a business process (e.g. Normalized vs Localized)
* Enumerate alternatives
  * copy/paste
    * maintenance is multiplied OR risk of outsync
    * dev UX = pain in the ass (code/file navigation)
  * multi-shape sub-type (e.g. Maybe Text, Either AddressId Address, Aeson.Value)
    * no safety based on the (business/tech) step (data could be invalid, could send unlocalized data back to front, etc.)
    * forced to handle impossible cases OR not handle (compilation-wise) possible cases
    * tough to understand what type of data for the reader
* A better alternative: Reusing a data structure in a type-safe way, with low boilerplate (aka Adapt a data structure along a (business / tech) process)
  * Intro to type families, nullary constructors (DataKinds), etc.
  * code migration
* conclusion
  * Going further
    * Also works for constructors, etc.
    * Trees That Grow