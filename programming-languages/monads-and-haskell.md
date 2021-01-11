Haskell uses monads for IO. Basically, it builds up an object tha
represents the transactions that should take place throughout the
program.

Do notation is just a shorthand to make this simpler.

I see this as kind of like CPS, actually.

## Common Haskell Classes

* Functor
    * anything with an `fmap :: Functor f => (a -> b) -> f a -> f b`.
    * Generalizes the concept of `map`.
    * Any "container" or box type thing that you can still apply a
      function to.
    * Simplest example is `Either`. `fmap (+ 3) (Just 3)` gives `Just
      6`. `fmap f Nothing` gives `Nothing`.
    * They also define `<$>`, which is an infix version. So `(+ 3) <$>
      (Just 3)` is `Just 6`.
    * Another example is `IO`. We define:
      * `fmap f action = do result <- action; return (f result)`.
    * Source: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html
* Applicative Functor
    * An `Applicative` is a `Functor that satisfies the following.
    * It has `<*> :: Functor f => f (a -> b) -> f a -> f b`.
    * Also comes with a `pure :: Functor f => a -> f a`.
      * Called 'lifting' a value.
    * `(pure (3+)) <*> (Just 5)` equals `Just 8`.
    * Here's a sophisticated example that uses `<$>`:
      * `+ <$> (Just 3) <*> (Just 5)`.
      * Remember that `+ <$> (Just 3)` is `Just (+ 3)`. Call this `f`.
        Then `f <*> (Just 5)` returns `Just (f 5)`.
    * Basically any "container" or box type thing that might itself
      hold a function, allowing you to use this function on further
      things.
    * `Either` can implement; apply a function included in `Just`,
      else return `Nothing`.
      * Another example: a tree of functions `a -> b`, to apply to each
        item in the tree of `a`s, producing a new tree of `b`s
      * And, `IO` implements `Applicative`. Because `(<*>) :: IO (a ->
        b) -> IO a -> IO b`. And we would define `a <*> b = do f <- a; x
        <- b; return (f x)`.
    * As a synonym, it defines `liftA2 :: (a -> b -> c) -> f a -> f b ->
      f c`.
        * We define `liftA2 f a b = f <$> a <*> b`.
        * This is the same as before, but perhaps makes it clear that we
          can apply binary functions to two applicatives, rather than
          just a unary function to a functor.
        * Basically, `liftA2` takes a binary function and lifts it.
    * Source: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html
* Monoids
    * Basically needs an `mempty` and a `mappend :: m -> m -> m`.
    * `Any` could be a monoid; `mappend True False = True`, etc.
    * Array is also a good example, using concatenation.
* Foldable
    * Implements things you can call `foldr` on.
    * Writes in terms of a `foldMap :: (Foldable f, Monoid m) => (a ->
      m) -> f a -> m`.
    * For a tree this would look like:

```
foldMap f Empty = mempty
foldMap f (Tree x l r) = foldMap f l `mappend`
                         f x `mappend`
                         foldMap f r
```

    * Note that you could easily use this to build an array
      representation of the tree. But it would also be easy to write:

```
getAny $ foldMap (\num Any (odd num)) myBigNumbersTree
foldMap (\num [num]) myBigNumbersTree
```

* Monads
    * `>>= :: (Monad m) => m a -> (a -> m b) -> m b`
    * This is exactly what is needed for IO, of course.
    * Also `>> :: (Monad m) => m a -> m b -> m b`.
    * Basically, sequences, but ignores previous.
    * `return :: (Monad m) => a -> m a`; this boxes a value.
    * We can think of this as an append log of commands.
    * Another possibility with lists; we can explore more and more
      possibilities, as in the knight's travails.
    * In lists, `>>= :: [a] -> a -> [b] -> [b]` and this is done by
      `>>= xs f = concat (map f xs)`.
* Writer
    * Equivalent to `newtype Writer m a = (a, m)`, where `m` is a
      monoid. It allows you to do computation while building up a log.

```
(Monoid m) => Monad (Writer m a) where
    (Writer (x, v)) >>= f = let (Writer y, v') = f x in (Writer y (v `mappend` v'))
```

* Also I saw (and wrote!) the State monad.

Haskell books dump:

I already (re)read Learn You a Haskell recently...

* http://www.apress.com/9781430262503
* http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell
* http://chimera.labs.oreilly.com/books/1230000000929
* http://www.cs.nott.ac.uk/~gmh/book.html
* http://www.haskellcraft.com/craft3e/Home.html
* http://www.yesodweb.com/book
* https://www.haskell.org/tutorial/
* https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Preamble
* https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
* https://wiki.haskell.org/Haskell
* http://dev.stephendiehl.com/hask
* http://www.vex.net/~trebla/haskell/index.xhtml
* https://wiki.haskell.org/Learning_Haskell
* https://wiki.haskell.org/How_to_write_a_Haskell_program
* https://wiki.haskell.org/Typeclassopedi
* https://news.ycombinator.com/item?id=10008169
