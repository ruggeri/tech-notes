## `Control.Monad`

We've previously generalized `Functor` to `Applicative`. But we could
have generalized in another direction. We could have asked: if a
function `f :: a -> m b` takes in an `a` and produces a monad wrapped
value of type `m b`, then how do we apply such a function to an already
wrapped value of type `m a`?

That is: `f` produces a boxed value. It's no good to try `f <$> boxedA`,
since then this would just produce a value of type `m (m b)`. Check it
out:

```haskell
Prelude> f :: Int -> IO Int; f x = pure (x + 1)
Prelude> ioX :: IO Int; ioX = pure 3
Prelude> :t (f <$> ioX)
(f <$> ioX) :: IO (IO Int)
-- Note: ghci will perform IO once for you, but not recursively.
Prelude> import System.IO.Unsafe (unsafePerformIO)
Prelude System.IO.Unsafe> unsafePerformIO (f <$> ioX)
4
```

Let's consider our standard examples of `[]`. How can we apply `f :: a
-> [b]` to a `[a]`? We could `fmap f listOfAs` to produce
`listOfListOfBs :: [[b]]`. Since `[]` is a `Monoid`, we can then call
`mconcat listOfListOfBs` to collapse to `listOfBs :: [b]`. This approach
should be appropriate for anything which is:

1. A `Functor` (so we can call `fmap`),
2. A `Monoid` (so that we can call `mappend` on a list of them to collapse),
3. A `Foldable` (so that we can call `fold`, which will call `mappend`;
   basically, we need to be able to treat the object like a list of
   things).

TODO: But I don't actually see an implementation for everything with
those type constraints? Why?

Anyway, let's look at the `Monad` typeclass:

```haskell
-- From GHC.Base
class Applicative m => Monad m where
  -- A way to lift a function into the monad, but not 'double-box' the
  -- result.
  (>>=) :: m a -> (a -> m b) -> m b

  -- A convenience built on >>= but ignores the previous value.
  (>>) :: m a -> m b -> m b
  ma >> mb = ma >>= \_ -> mb

  -- return is just a synonym for pure
  return = pure
```

And let's see some implementations:

```haskell
-- Unbox and apply. Like `fmap`, but, as ever, avoiding the double
-- boxing.
instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing >>= _ = Nothing

instance Monad [] where
  -- Use a list comprehension to produce a flattened list.
  xs >>= f = [y | x <- xs, y <- f x]
```

You can use `do` notation in place of `return` and `>>=`. Consider this
scenario:

```haskell
-- Squares a number, but doesn't let it get too big.
squareIfNotTooBig :: Int -> Maybe Int
squareIfNotTooBig n = if squareN < 40 then Just squareN else Nothing
  where
    squareN = n * n

-- Quintuples a number, but doesn't let it get too big.
quintupleIfNotTooBig :: Int -> Maybe Int
quintupleIfNotTooBig n = if fiveN < 40 then Just fiveN else Nothing
  where
    fiveN = 5 * n

-- Tries to do both, but returns Nothing if either calculation failed.
doBoth :: Int -> Maybe (Int, Int)
doBoth n =
  (squareIfNotTooBig n) >>=
  \squareN -> (quintupleIfNotTooBig n) >>=
  \fiveN -> return (squareN, fiveN)

-- Same thing, written more simply
doBoth n = do
  squareN <- squareIfNotTooBig n
  fiveN <- quintupleIfNotTooBig n
  -- return boxes it in a Just for us.
  return (squareN, fiveN)
```

It's a little weird, but note that this would work:

```haskell
import Data.Function ((&))

xs = [10, 20, 30]

f x = [x, 2*x, 3*x]

ys = do
  -- Note that `xs` contains *many* `x` values, not just one.
  x <- xs
  -- The list will be reduced
  f x

-- ys == [10, 20, 30, 20, 40, 60, 30, 60, 90]
```

But then you also have some weirdness:

```haskell
-- Fine, works normally and just ignores the single `y` value.
ys = do
  x <- xs
  -- Could also simply write [1]. This is the same, but doesn't make the
  -- monad value available to succeeding computations.
  y <- [1]
  f x

-- Equivalent to:
ys = xs >>= (\x -> [1] >>= (\_ -> f x))
-- Or also equivalent, since we don't use the `y` value
ys = xs >>= (\x -> [1] >> f x)

-- This seeming irrelevancy forces ys == []
ys = do
  x <- xs
  y <- []
  f x

-- Equivalent to:
ys = xs >>= (\x -> [] >>= (\_ -> f x))
ys = xs >>= (\x -> [] >> f x)

-- Will cause each f x to be repeated twice.
ys = do
  x <- xs
  y <- [1, 2]
  f x
```

The `[]` monad is odd because the same function might be called multiple
times with different input values. Which is quite unlike `IO` monad,
where the continuation function is called at most once (exactly once if
no preceding step failed).

The Learn You A Haskell people talk about how list comprehensions are
syntactic sugar for `do` syntax. I've shown this. But there can also be
*guards* in list comprehension:

```haskell
-- Only return those numbers with a 7 in the base 10 representation.
[ x | x <- [1..50], '7' `elem` show x ]
```

How is this accomplished? The list is actually an instance of
`MonadPlus`. `MonadPlus` means a `Monad` which is also a `Monoid`. In
particular, there's an `mzero` function in the typeclass for
`MonadPlus`: it's the equivalent of the `Monoid` `mempty` function. In
the case of `[]`, this is the empty list. You can see the definition for
`guard` in the `Control.Monad` source file.

Here's my implementation of Knight's Travails.

```haskell
import Data.Function ((&))
import Control.Monad (guard)

type Position = (Int, Int)

isValidPosition :: Position -> Bool
isValidPosition (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

nextPositions :: [Position] -> [Position]
nextPositions currentPositions = do
  (x, y) <- currentPositions
  nextPosition <- [(x - 2, y - 1), (x - 2, y + 1),
                   (x - 1, y - 2), (x - 1, y + 2),
                   (x + 1, y - 2), (x + 1, y + 2),
                   (x + 2, y - 1), (x + 2, y +1)]
  -- Notice the use of guard to fail any position that is not valid.
  guard (isValidPosition nextPosition)
  return nextPosition

reachablePositions :: [Position] -> Int -> [Position]
reachablePositions currentPositions 0 = currentPositions
reachablePositions currentPositions numSteps = do
  reachablePositions (nextPositions currentPositions) (numSteps - 1)

isReachable :: Position -> Position -> Int -> Bool
isReachable startPosition endPosition maxNumSteps =
  endPosition `elem` (reachablePositions [startPosition] maxNumSteps)

main = do
  putStrLn ((isReachable (1, 1) (8, 8) 7) & show)
```

We've seen that `monadicValue >>= f` feeds the value into the function
`f :: a -> b`. And of course we can write: `monadicValue >>= f >>= g`.
But is there a way to "compose" `f, g` easily, like we did with `f . g`
for normal functions?

Answer: the analogue is `f >=> g`. We can write: `monadicValue >>= (f >=> g)`.

* Source:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monad
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html
