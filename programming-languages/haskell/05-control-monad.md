# `Control.Monad`

We previously generalized `Functor` to `Applicative`. This let us apply
a value of `m (a -> b)` to a value of `m a`, producing a value of `m b`.

The simple examples are `Maybe` or `Either` or even `IO` (which we
haven't met yet). A fancier example was `[]`: we learned how to apply
`[a -> b]` to `[a]` to produce `[b]`.

The other way we could try to generalize functor is to ask: how should
we apply `a -> m b` to `m a`. The `<*>` already allows us to `mf <*> mx`
to produce an object of type `m (m b)`. But this is often not what we
want. This is called *double boxing*.

## `[]`: Non-Deterministic Computation Example

Consider our non-deterministic computation example. We consider `<*>`
for applying an ensemble of deterministic functions to an ensemble of
inputs. What about a non-deterministic function? For instance: `f: a ->
[b]`. If we `f <*> xs`, we're going to get a value of type `[[b]]`,
which is double-boxed.

Let's define:

```haskell
-- My own >>= implementation for lists.
>>= :: (a -> [b]) -> [a] -> [b]
>>= f xs = fold (f <*> xs)
```

What have I done here? `mconcat` will flatten the `[[b]]` to just a
`[b]` value. Which creates duplication of input states, but is not
incorrect. It could even be useful for computing the probability of the
non-deterministic computation computing the correct answer.

Let's reflect on what I've used here:

0. `[]` is `Applicative`: it has a `<*>` method
0. `[]` is `Foldable`: it has a `fold` method
0. `[]` is `Monoid`: you can aggregate `[]` values through
   concatenation.

This is more structure than we'll have for most `Monad`s. But there is a
sense in which, whenever feed a monadic value to a function `f` that
takes the prior value and produces a new monadic value, there's a sense
in which we are folding two states (the previous state and the next
state) into one.

## Definition of `Monad`

Let's look at the definition of `Monad`:

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

## Implementations for `Maybe`, `[]`

```haskell
--These are from GHC.Base

-- Unbox and apply. Like `fmap`, but, as ever, avoiding the double
-- boxing.
instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing >>= _ = Nothing

instance Monad [] where
  -- Use a list comprehension to produce a flattened list.
  xs >>= f = [y | x <- xs, y <- f x]
```

## `do` Notation

You can use `do` notation in place of `return` and `>>=`. Consider this
example:

```haskell
tooBig = 40

-- Squares a number, but doesn't let it get too big.
squareIfNotTooBig :: Int -> Maybe Int
squareIfNotTooBig n = if squareN < tooBig then Just squareN else Nothing
  where
    squareN = n * n

-- Quintuples a number, but doesn't let it get too big.
quintupleIfNotTooBig :: Int -> Maybe Int
quintupleIfNotTooBig n = if fiveN < tooBig then Just fiveN else Nothing
  where
    fiveN = 5 * n

-- Tries to do both, but returns Nothing if either calculation failed.
-- A little confusing/hard to read.
doBoth :: Int -> Maybe (Int, Int)
doBoth n =
  (squareIfNotTooBig n) >>= \squareN ->
    ((quintupleIfNotTooBig n) >>= \fiveN -> return (squareN, fiveN))
```

That's kind of clunky, so we might prefer this `do` notation syntax:

```haskell
-- Same thing, written more simply
doBoth n = do
  squareN <- squareIfNotTooBig n
  fiveN <- quintupleIfNotTooBig n
  -- return boxes it in a Just for us.
  return (squareN, fiveN)
```

## 'Side effects' in `do` notation

Sometimes it looks like a computation has side-effects.

```haskell
doBoth n = do
  squareN <- squareIfNotTooBig n
  fiveN <- quintupleIfNotTooBig n
  -- Actually, let's just always return Nothing.
  Nothing
  return (squareN, fiveN)
```

The `Nothing` line looks irrelevant. The value is not bound (btw,
there's no value *to* bind). But remember how this translates:

```haskell
doBoth n =
  (squareIfNotTooBig n)
  >>= \nSq -> ((quintupleIfNotTooBig n)
    >>= \nFive -> (Nothing
      >> (return (nSq, nFive))
    )
  )
```

You can see that `Nothing` is part of the chain of binds, and thus will
unavoidably cause `Nothing` to be the result.

What we see here is that `>>=` doesn't just care about the *value* that
is exposed by `<-`. It also cares about the monad's *context*. So an
unused line of `Nothing` is very different from an unused `Just ()`
line, even if neither value gets bound by `<-` nor used.

## Weird `[a]` Examples

Here's an example non-deterministic computation:

```haskell
xs = [10, 20, 30]
f x = [x, 2*x, 3*x]
ys = do
  x <- xs
  f x
-- ys == [10, 20, 30, 20, 40, 60, 30, 60, 90]
```

This won't change anything:

```haskell
ys = do
  x <- xs
  -- Doesn't really matter. Value will be ignored.
  [1]
  f x
```

But this matters:

```haskell
-- Will cause each f x to be repeated twice.
ys = do
  x <- xs
  -- Basically, each `x` is being mapped (non-deterministically) to two
  -- intermediate states z = 1, z = 2. These states are not used, but
  -- they do create a branching point.
  [1, 2]
  f x
```

* Source:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#Monad
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html
