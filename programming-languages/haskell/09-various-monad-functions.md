# Various Monad Functions

## Basic Monad Functions

```haskell
-- liftM is a synonym for `fmap`
liftM :: Monad m => (a -> b) -> m a -> m b
liftM = fmap

-- ap is a synonym for `<*>`. Just as there is liftA2, there is also
-- liftM2 et cetera.
ap :: Monad m => m (a -> b) -> m a -> m b
ap = <*>

-- join flattens a monad wrapped in itself.
join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id

-- Equivalently, we could write `>>=` in terms of `join`. That's what I
-- was getting at in my discussion of how a Monad is similar to
-- something foldable.
(>>=) :: m a -> (a -> m b) -> m b
mx >>= f = join (fmap f mx)
```

## Monads and Lists

Now they start talking about various analogues of `List` functions.

### `filterM`

Here's my definition:

```haskell
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x:xs) = do
  -- Worth noting: if you flip around these two statements, you'll be
  -- asked about elements in reverse order. The reason why is
  -- demonstrated in the `>>=` version below.
  keepCurrentX <- p x
  restOfXs <- filterM p xs
  if keepCurrentX then return (x:restOfXs) else return restOfXs

-- For practice/interest, written using `>>=`.
otherFilterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
otherFilterM p [] = return []
otherFilterM p (x:xs) = p x >>= (
  \keepCurrentX ->
    let
      restOfXs = filterM p xs
    in
      if keepCurrentX
      then restOfXs >>= \xs -> (return (x:xs))
      else restOfXs
  )
```

Here's an example of using `filterM`

```haskell
askIfShouldKeep :: Int -> IO Bool
askIfShouldKeep x = do
  putStrLn ("Should we keep the number " ++ (show x) ++ "?")
  input <- getLine
  case input of
    "yes" -> return True
    "no" -> return False
    _ -> askIfShouldKeep x

-- Note: because IO evaluates eagerly/strictly, you'll be asked about
-- all elements even if only one is desired.
main = do
  keptValues <- filterM askIfShouldKeep [10, 20, 30]
  putStrLn (show keptValues)
```

For reference, here is the standard definition of `filterM`:

```haskell
-- You can see it's written without the `xs`. It's written using
-- `foldr`. The starting value is the empty monad `[]`.
filterM p = foldr (
  -- Next question is: how do we accumulate values? Well, it depends on
  -- whether the next value `x` matches the predicate. Thus we will use
  -- `liftA2` to feed `p x` into a function which decides how we will
  -- accumulate.
  \ x -> liftA2 (
    -- Here, we'll give the function `x:` if we want to keep `x`. When
    -- passed `acc`, `x:` will prepend. Else we want `id`, to preserve
    -- `acc` as it is.
    \ flg -> if flg then (x:) else id
  ) (p x)
) (pure [])
```

### `mapM`, `forM`, `sequenceM`

The monad analogue for `mapM`:

```haskell
-- Notice that if `>>=` does not strictly evaluate the prior value
-- (like `ys`) before it is needed, you can safely apply `mapM` to an
-- infinite list if you only plan to take a few elements.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return (y:ys)

-- alternatively
mapM _ [] = return []
mapM f (x:xs) = (f x) >>= \y -> (
  mapM f xs >>= \ys -> return (y:ys)
  )

-- forM is just a flipped version of mapM
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs f = mapM f xs

-- mapM_ and forM_ do the mapping, but don't collect up the results in a
-- list. They throw them away and return a value of type `m ()`. You use
-- these when all you want is the side-effects and don't want to waste
-- memory.

-- 'sequences' a list of monads.
sequence :: Monad m => [m a] -> m [a]
sequence = mapM id
```

Here are two examples where `mapM` does terminate:

```haskell
-- simplistic; once `Left` is encountered, no more computation is done.
mapM (\x -> if x*x == 100 then Left x else Right x) [1..]

-- Fancier. It *could* have computed more, but we only used the first
-- element.
import Control.Monad.Writer
import Data.Function ((&))

type LogWriter = Writer [String]

logs :: LogWriter [()]
logs = mapM (\ _ -> tell ["Hello world!"]) [1..]

-- Terminates! This must be that `mappend` needs only produce its
-- first argument.
main = do
  putStrLn ((execWriter logs !! 0) & show)
```

## `forever`, `replicateM`

```haskell

-- Returns a monadic value that performs the same operation over and
-- over. Note: depending on how the monadic *context* is evolving, and
-- whether there is any fixed-point, you may be able to totally evaluate
-- the returned value.
forever :: Monad m => m a -> m a
forever m = m >> (forever m)

-- Perform the action `n` times.
replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n m = liftM2 (:) m (replicateM (n - 1) m)
```

## `foldM`

* **TODO**: Finish writing up explanation of `foldM`.
* `foldM :: (b -> a -> m b) -> [a] -> m b` works like the others.
* `foldM_` ignore the result (wrapped value). They are used for
  side-effects/context, I assume.

* Source:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html
