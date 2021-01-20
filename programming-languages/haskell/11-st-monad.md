# `Control.Monad.ST`

## `IORef`

```haskell
import Control.Monad (forM_)
import qualified Data.IORef as IORef

-- This is going to use mutable state to do the sum computation.
sumIO :: Num a => [a] -> IO a
sumIO as = do
  -- Creates a memory cell in which zero is stored.
  sumCell <- IORef.newIORef 0

  -- Sequences a bunch of IO actions that modify the IO ref
  forM_ as $ \x -> do
    -- Could more simply use the helper modifyIORef. But being explicit
    -- for clarity.
    currentSumVal <- IORef.readIORef sumCell
    IORef.writeIORef sumCell (x + currentSumVal)

  -- Pull out the final value.
  IORef.readIORef sumCell

main = do
  result <- sumIO ([1, 2, 3])
  putStrLn (show result)
```

## `STRef`

It's annoying that `sumIO` must return `IO a`. It ought to be callable
from pure code. This is the purpose of the `ST` monad and `STRef`. Let's
look at some functions:

```haskell
-- From GHC.STRef.
--
-- Takes an initial value `a`. Creates an `ST` action which creates an
-- instance of `STRef`.
--
-- Notice the 'dummy' type `s`. The type `s` is going to be specific to
-- a single `ST` instance. And note that `STRef` is 'tagged' with the
-- type `s`, too. This will bethe inseparable connection between the
-- `ST` value and the `STRef` value.
newSTRef :: a -> ST s (STRef s a)
```

```haskell
-- From GHC.STRef.
--
-- We can read from the STRef. Notice that we can only perform the read
-- inside the same `ST` monad that `STRef` was created in.
readSTRef :: STRef s a -> ST s a

-- And, likewise, we can write to the STRef. Again, the `ST` value that
-- represents the write action is of the same type that the `STRef` was
-- created from.
writeSTRef :: STRef s a -> a -> ST s ()
```

```haskell
-- From Data.STRef
--
-- A helper that reads, runs a function, and writes.
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  value <- readSTRef ref
  writeSTRef ref (f value)
```

## `runST` Example

We now explore `ST` and `runST`. Here's an example of using it:

```haskell
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef

-- Schedules an action to allocate space to hold an initialized
-- variable.
makeX :: ST.ST s (STRef.STRef s Int)
makeX = STRef.newSTRef (-100)

-- Ditto
makeY :: ST.ST s (STRef.STRef s Int)
makeY = STRef.newSTRef 100

-- Schedules a number of actions...
addVars :: ST.ST s Int
addVars = do
  -- Create two variables.
  xRef <- makeX
  yRef <- makeY
  -- Read their initial values.
  xValue <- STRef.readSTRef xRef
  yValue <- STRef.readSTRef yRef
  -- Produce a result.
  return (xValue + yValue)

main = do
  -- ST.runST executes the series of actions.
  let result = ST.runST addVars
  putStrLn (show result)
```

## `forall`

The real question for us is: what is the type of `ST.runST`? It is:

```haskell
runST :: (forall s. ST s a) -> a
```

What does this mean? It means that `runST` must take in a value which
can be evaluated as *any* kind of `ST s a`. `runST` demands that it be
able to assign `s` any type it wants.

Imagine if we tried to give `runST` a value of `ST s (STRef s a)`. Then
`runST` must return a value of type `STRef s a`. But this `s` type
parameter is no longer free; it was supposed to be specified to `runST`.

## Example

```haskell
{-# LANGUAGE RankNTypes #-}

-- Here's my box with a dummy parameter.
data MyBox s a = MyBox a

-- Puts a value in a box, but does not specify what `s` should be.
wrap :: a -> MyBox s a
wrap x = MyBox x

-- This puts double wraps, but in a tricky way. It requires that the
-- inner box be tagged with the same `s` type as the outer box.
doubleWrap :: a -> MyBox s (MyBox s a)
doubleWrap x = MyBox (MyBox x)

-- This unwrap function wants to make sure that no one impinges on its
-- ability to choose `s` for `MyBox s a`.
unwrap :: (forall s. MyBox s a) -> a
unwrap (MyBox x) = x

unwrapUnsafe :: (MyBox s a) -> a
unwrapUnsafe (MyBox x) = x

main = do
  -- let x = (wrap 3)

  -- Works
  -- let x = unwrapUnsafe (unwrapUnsafe (doubleWrap (3 :: Int)))

  -- Works
  -- let x = unwrap (unwrapUnsafe (doubleWrap (3 :: Int)))

  -- Sadly, this does not appear to work. It's true that `s` would
  -- escape its bound, but no one ever wants to further restrict the
  -- value of `s`.
  -- let x = unwrap (doubleWrap (3 :: Int))

  -- This could never work, because the 2nd call to unwrap is given a
  -- `MyBox s a`, but `s` is not at all free, because we already gave
  -- the first call to unwrap the right to choose the type of `s`.
  let x = unwrap (unwrap (doubleWrap (3 :: Int)))
  return ()
```

Here's the compiler error:

```
>> stack runghc test.hs

test.hs:35:19: error:
    • Couldn't match type ‘a’ with ‘MyBox s Int’
        because type variable ‘s’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a type expected by the context:
          forall s. MyBox s a
        at test.hs:35:18-40
      Expected type: MyBox s a
        Actual type: MyBox s (MyBox s Int)
    • In the first argument of ‘unwrap’, namely
        ‘(doubleWrap (3 :: Int))’
      In the expression: unwrap (doubleWrap (3 :: Int))
      In an equation for ‘x’: x = unwrap (doubleWrap (3 :: Int))
    • Relevant bindings include x :: a (bound at test.hs:35:7)
   |
35 |   let x = unwrap (doubleWrap (3 :: Int))
   |                   ^^^^^^^^^^^^^^^^^^^^^
```
