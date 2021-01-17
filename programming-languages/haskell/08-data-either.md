# `Data.Either`

The `Either` monad is used to do a series of computations, any of which
might fail. It's like the `Maybe` monad, except the `Left` data
constructor takes a value that represents what error was encountered.
Here is my implementation:

```haskell
-- Left, as the sinister constructor, will represent an 'error.'
data Either a b = Left a | Right b
  deriving Show

instance Functor (Either a) where
  -- Easy. Apply to the boxed value, if everything is all Right.
  fmap :: (b -> c) -> (Either a b) -> (Either a c)
  f `fmap` (Right x) = Right (f x)
  _ `fmap` (Left x)  = Left x

instance Applicative (Either a) where
  -- Lift the value up into the monad.
  pure :: b -> (Either a b)
  pure x = Right x

  -- I guess we'll throw away the 2nd error if there was an error with
  -- the function we're trying to <*> with.
  (<*>) :: (Either a (b -> c)) -> (Either a b) -> (Either a c)
  (Left x) <*> y = Left x
  (Right f) <*> (Left x) = Left x
  (Right f) <*> (Right x) = Right (f x)

instance Monad (Either a) where
  return :: b -> (Either a b)
  return = pure

  (>>=) :: (Either a b) -> (b -> (Either a c)) -> (Either a c)
  (Left x) >>= _ = Left x
  (Right x) >>= f = f x
```

Here's an example of our `Either` monad in action:

```haskell
doErrorComputation :: Either String Int
doErrorComputation = do
  x <- return 3
  y <- return 4
  -- As soon as an error occurs, it's all over.
  Left "I messed up"
  return (x + y)

main = do
  putStrLn (doErrorComputation & show)
```

* Sources:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.Either.html#Either
