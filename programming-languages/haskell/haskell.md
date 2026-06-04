This is a list of libraries and extensions things I could study in
Haskell. But I'm pretty anti-Haskell at this time.

* ST Monad <<
* Lens <<
* RankNTypes <<
* Multi parameter type classes <<
    * used for mtl, which is a very popular library
* GADT <<
* FunctionalDependencies
* TypeFamilies
* NamedFieldPuns and RecordWildCards
* Template Haskell

Other useful:

* Multi-way if
* Pattern guards

## GADT

Remember that an ADT lets you write:

```
data Tree a = Empty
            | Leaf a
            | Vertex (Tree a) (Tree a)
```

Note that these type constructors can be used to construct any kind of
`Tree a`, no matter what `a` is.

GADT lets you tweak that. It says: you can use this constructor only
on certain input types, and it will only produce certain kinds of
output types. A good example is:

```
data Expression a where
  I :: Int -> Expression Int
  B :: Bool -> Expression Bool
  Add :: Expression Int -> Expression Int -> Expression Int
  -- Notice this type constraint!
  Eq :: Eq a => Expression a -> Expression a -> Expression Bool
```

You can also use this to create wrapper types:

```
data Showable where
  MkShowable :: Show a => a -> Showable

showShow :: Showable -> String
showShow (MkShowable a) = show a
```

## Rank-N Polymorphism

Here's an example of rank 1 polymorphism:

```
id :: a -> a
id x = x
```

You can be more explicit if you turn on the Rank-N polymorphism module:

```
{-# LANGUAGE RankNTypes #-}
id :: forall a. a -> a
id x = x
```

This is making clear that `id` is a family of functions. Let's use a
type definition:

```
type IdFunc = forall a. a -> a
id :: IdFunc
id x = x
```

## Links

https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
https://ocharles.org.uk/blog/posts/2014-12-02-view-patterns.html
https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html
https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html
https://ocharles.org.uk/blog/posts/2014-12-10-nullary-type-classes.html
https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html
https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html
https://ocharles.org.uk/blog/posts/2014-12-14-functional-dependencies.html
https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html
https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html
https://ocharles.org.uk/blog/guest-posts/2014-12-21-arrows.html

https://wiki.haskell.org/Rank-N_types

http://dev.stephendiehl.com/hask

https://mail.haskell.org/pipermail/haskell-cafe/2012-June/101539.html

https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions

http://stackoverflow.com/questions/10845179/which-haskell-ghc-extensions-should-users-use-avoid
