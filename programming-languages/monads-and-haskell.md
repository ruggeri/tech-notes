Haskell uses monads for IO. Basically, it builds up an object tha
represents the transactions that should take place throughout the
program.

Do notation is just a shorthand to make this simpler.

I see this as kind of like CPS, actually.

## Common Haskell Classes

* Functor
    * anything with an `fmap :: Functor f => (a -> b) -> f a -> f b`.
    * Any "container" or box type thing that you can still apply a
      function to.
    * `Either` can implement; apply the function to `Just`, ignore for
      `Empty`.
* Applicative Functor
    * Anything with a `<*> :: Functor f => f (a -> b) -> f a -> f b`.
    * Also comes with a `pure :: Functor f => a -> f a`.
    * `(pure (3+)) <*> (Just 5) == Just 8`.
    * As a convenience, could write `+ <$> Just 3 <*> (Just 5)`, as
      `<$> :: (a-> b) -> f a -> f b`.
    * Basically any "container" or box type thing that might itself
      hold a function, allowing you to use this function on further
      things.
    * `Either` can implement; apply a function included in `Just`,
      else return `Empty`.
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
