## `Control.Monad.Writer` Monad

The idea of the `Writer` monad is that it works on a `Monoid`
'accumulator', and the `>>=` operation works by fusing the two
accumulators through `mappend`.

The `Writer` monad is defined in the `transformers` package. It's
written in a way that uses a concept called 'monad transformers' and as
such is a little inscrutable to me at present. However, I do write my
own version below:

```haskell
-- Allow re-declaration of instance signatures to help me debug.
{-# Language InstanceSigs #-}

import Data.Function ((&))

-- Great naming. `w` means the accumulator value. `a` means the kind
-- of value wrapped by the monad.
newtype Writer w a = Writer { runWriter :: (a, w) }

-- This seems pretty clear. If you want to transform the wrapped
-- value without writing into the accumulator, go for it. Note: we write
-- just `Writer w` without any kind of wrapped value type `a` because
-- Functor has kind * -> *. That makes sense, because the whole point is
-- that the wrapped value type will change after function application.
instance Functor (Writer w) where
  fmap :: (a -> b) -> (Writer w a) -> (Writer w b)
  f `fmap` wrappedX = Writer (y, ctx)
    where
      (x, ctx) = runWriter wrappedX
      y = f x

instance Monoid w => Applicative (Writer w) where
  -- It's easy to lift a value into the Writer (wrap it with the
  -- writer), so long as there is a `mempty` value that represents a
  -- natural start state for the accumulator.
  pure :: a -> (Writer w a)
  pure x = Writer (x, mempty)

  -- TODO: This seems very weird. Not sure I understand what this should
  -- be doing....
  (<*>) :: (Writer w (a -> b)) -> (Writer w a) -> (Writer w b)
  wrappedF <*> wrappedX = error "What the fuck?"

-- Same note about use of `Writer w`: `>>=` will transform the wrapped
-- type.
instance Monoid w => Monad (Writer w) where
  (>>=) :: (Writer w a) -> (a -> Writer w b) -> (Writer w b)
  wrappedX >>= f = Writer (y, oldCtx `mappend` newCtx)
    where
      (x, oldCtx) = runWriter wrappedX
      (y, newCtx) = runWriter (f x)
```

The `Writer` monad provides several methods for evaluation:

* `runWriter`: unwraps `(value, accumulation)`.
* `execWriter`: tosses away `value` and just returns
  `accumulation`. Also: it evaluates strictly (forces evaluation). This
  forces the writer operation to run completely.
* `mapWriter`: transforms a `Writer w a` to a `Writer w' b`. You provide
  a function to map the internal state `(a, w) -> (b, w')`.

Here is an example use:

```haskell
-- Just a convenient synonym.
type Logged x = Writer [String] x

addOne :: Int -> Logged Int
addOne x = Writer (x + 1, ["Added one"])

double :: Int -> Logged Int
double x = Writer (x + x, ["Doubled"])

-- Do a series of computations, collecting up the list of logs.
doSomeMath :: Int -> Logged Int
doSomeMath x = do
  x1 <- addOne x
  x2 <- double x1
  x3 <- addOne x2
  return x3

main = do
  putStrLn ((doSomeMath 10) & runWriter & show)
```

* Sources:
  * The `Writer` monad is written in terms of monad transformers. It is
    defined in the `transformers` package, which is a dependency of the
    `mtl` package. I believe the `mtl` package extends `trasformers`.
  * Here is the source code for `Writer`'s monad implementation:
  * https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Writer.Lazy.html#line-197
* TODO: learn about Monad transformers!
