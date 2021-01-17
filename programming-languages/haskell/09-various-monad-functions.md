# Various Monad Functions

**TODO**: Review me!

* Basic monad functions:
  * `runState`/`runWriter`: they return the `(value, context)`.
  * `evalState`/: returns just the value.
  * `execState`/`execWriter`: returns just the context.

* `liftM` is a synonym of `fmap`. It lifts applies a function to a
  wrapped value.
* `ap` is a synonym for `<*>`. It's used when you have a wrapped
  function.
  * Just as there is a `liftA2` function for applying `a -> b -> c` to
    values of type `m a`, `m b` (defined in terms of `ap`), so there are
    `liftM2`, `liftM3`, etc.
* The `join` function flattens a monad wrapped in itself. It is defined
  as `mmx >>= id`. Here `mmx :: (m (m a))`, and `id :: m a -> m a`.
  * If you didn't have monads, then you'd end up with this kind of
    wrapping explicitly. The whole point of monads is that you can
    flatten them, and that the context accumulates across the bind.
  * They make the point that `m >>= f` is always exactly `join (fmap f
    m)`.

```haskell
join (Just (Just 0)) -- 0
join (Just Nothing) -- Nothing

-- Example of double-wrapping and join of the State monad.
import Control.Monad.State
import Data.Function ((&))

initializeStack :: State [Int] ()
initializeStack = state $ \s -> ((), [0, 0, 0])

pushOntoStack :: Int -> State [Int] ()
pushOntoStack x = state $ \s -> ((), x:s)

doubleWrappedMonad :: State [Int] (State [Int] ())
doubleWrappedMonad = do
  initializeStack
  return (pushOntoStack 5)

main = do
  putStrLn (
    let
      singleWrappedMonad = (join doubleWrappedMonad)
      result = execState singleWrappedMonad []
    in
      show result
    )
```

Now they start talking about various analogues of `List` functions:

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

askIfShouldKeep :: Int -> IO Bool
askIfShouldKeep x = do
  putStrLn ("Should we keep the number " ++ (show x) ++ "?")
  input <- getLine
  case input of
    "yes" -> return True
    "no" -> return False
    _ -> askIfShouldKeep x

main = do
  keptValues <- otherFilterM askIfShouldKeep [10, 20, 30]
  putStrLn (show keptValues)
```

For reference, here is the standard definition of:

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

* `mapM` can be written practically the same way.

```haskell
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
```

Here is a question: are `filterM` and `mapM` strict? That is: will
`mapM f [1..]` ever return?

I believe that depends on the behavior of the `>>=` function. Here is an
example where `mapM` does terminate:

```haskell
Prelude> mapM (\x -> if x*x == 100 then Left x else Right x) [1..]
Left 10
```

That cheats a little, because after encountering `Left x`, `>>=` is a
no-op. But also observe this example:

```haskell
import Control.Monad.Writer
import Data.Function ((&))

type LogWriter = Writer [String]

logs :: LogWriter [()]
logs = mapM (\ _ -> tell ["Hello world!"]) [1..]

-- Terminates! This must be that `mappend` is needs only produce its
-- first element
main = do
  putStrLn ((execWriter logs !! 0) & show)
```

* There are other operations defined similarly.
* `sequence = mapM id`. All it does is convert `[m a]` to `m [a]`.
  * Note that, per above, this *sequences* the monadic actions, but does
    not necessarily ensure that all monadic actions will be executed!
* `forM` is just a version of `mapM` with arguments flipped. `forM ::
  [a] -> (a -> m b) -> m [b]`.
* `foldM :: (b -> a -> m b) -> [a] -> m b` works like the others.
* There are various `mapM_`, `forM_`, `foldM_` that ignore the result
  (wrapped value). They are used for side-effects/context, I assume.
* `forever m = m >> (forever m)`. Effectively, this runs the monadic
  action infinitely. As ever, `forever` *might stop* depending on how
  `>>=` is used. For instance, perhaps the monad is over file
  operations, and `>>=` continues to perform file operations until a
  command `Exit` is given.
* `replicateM :: Int -> m a -> m [a]` is like `forever` but replicates
  only `n` times.

* Here are the conditional ones:
  * An `Alternative` is an `Applicative` with an additional `empty`.
    `empty` symbolizes a failure value like `Nothing`.
  * Importantly, `Alternative` has a `<|>` operation which is like `||`.
  * Basically, `Alternative` is generalizing the idea of a bool. Some
    values are truthy, and others (special one) is Falesy.
  * `guard :: Alternative f => Bool -> f ()`. This returns `empty` if
    the bool is False. Else it returns `pure ()` (the most basic form of
    truthy value).
  * `when :: Applicative f => Bool -> f () -> f ()`. A generalization of
    `guard` to `Applicative`. Here you specify the default value for
    false. Else it will return `pure ()` when the bool is false.
    * Useful for `when debugMode (putStrLn "we reached this line!")`.
  * `unless` is simply the reverse of `when`.

* Other:
  * `zipWithM`
  * `>=>`

* Source:
  * https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Control.Monad.html
