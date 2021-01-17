# `Control.Applicative` Functor

We say *Applicative Functor*, but the Haskell typeclass is named
`Applicative`. An `Applicative` extends the concept of a `Functor` by
allowing that the `Functor` might wrap a function, which in turn can be
applied.

```haskell
-- From GHC.Base
class Functor f => Applicative f where
    -- Lifts a value into the Applicative.
    pure :: a -> f a
    -- Takes a function wrapped in an Applicative and applies it to
    -- another applicative.
    (<*>) :: f (a -> b) -> f a -> f b
```

Here's an example with `IO`:

```haskell
import Data.Functor ((<&>))

-- Define functions that double/triple
double x = 2 * x
triple x = 3 * x

-- Let user select which function to use.
selectDoubleOrTriple :: IO (Int -> Int)
selectDoubleOrTriple = do
  putStrLn "Choose to double or triple a number"
  option <- getLine
  return (
    if (option == "double") then double else triple
    )

-- Notice the use of `<*>`. Also notice the use of `pure` to lift the
-- value 3.
doubleOrTripleThree :: IO Int
doubleOrTripleThree = selectDoubleOrTriple <*> (pure 3)

-- Last, notice the use of `<&>`. This is simply the flip of `<$>` and
-- thus lifting the second argument of &.
main :: IO ()
main = (doubleOrTripleThree <&> show) >>= putStrLn
```

The implementation of the typeclass `Applicative` for `IO` is a little
too much for me to understand right now (especially because I don't
understand IO/do notation yet)!

A perhaps simpler example with the `Maybe` `Applicative`:

```
x = (Just show) <*> (Just 3)
y = (Nothing :: (Maybe (Int -> String))) <*> (Just 3)
z = (Just (show :: Int -> String)) <*> Nothing

main = do
  putStrLn (show x)
  putStrLn (show y)
  putStrLn (show z)
```

Here we see the implementation:

```haskell
-- From GHC.Base
instance Applicative Maybe where
    -- Simply wrap the value.
    pure = Just

    -- To use <*>, unwrap `f` if any, and then delegate to `fmap`.
    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing
```

There is a synonym of `<*>`, `liftA2`:

```haskell
-- This is defined in the typeclass `Applicative` in case you have a
-- more efficient specialization.
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = (fmap f x) <*> y

--They contrast this with `liftA`, defined in `GHC.Base`:
liftA :: (a -> b) -> f a -> f b
liftA f x = f <$> x
```

The Learn You A Haskell folks say that the difference between Functor
and Applicative is made clear by `liftA` vs `liftA2`. They note that
only an applicative could have `liftA2` defined, since `liftA2 f x` can
produce an intermediate result of type `Applicative (b -> c)`.

## `[]` as `Applicative`

Last, let's look at something which is a little weird. Let's consider
the implementation of `Applicative` for `[]`.

```haskell
instance Applicative [] where
  -- Duh.
  pure x = [x]

  -- Notice that this doesn't work element-wise. In particular, we
  -- expect of all `Applicative`s that `(pure id) <*> v == v`.
  fs <*> xs = [f x | f <- fs, x <- xs]
```

This is certainly a fancier notion of `Applicative` than most types like
`Maybe`, `Either`, `IO`, et cetera, which are really just a box of at
most one function value.

I'm not sre when I would want to use this. One idea is: if `[a]`
represents a set of possibilities, then `fs` is a set of possible
(deterministic) functions, while `xs` is a set of possible inputs. Then
`fs <*> xs` is the set of possible outputs.

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Applicative.html
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Applicative
