# `Control.Monad.State` Monad

The `Writer` monad is a little simplistic. The context for a value is
updated simply through concatenation. We can produce new context values,
which are reduced into the current accumulation through `mappend`.

We don't have the ability to ever look at the context that has been
collected along the way. We must wait until someone calls `runWriter` to
'cash out' the accumulated value at the end.

Thus, it is entirely appropriate that we call `Writer` 'writer.' It lets
us *write* into a log, which we cannot read at any intermediate step. In
fact, it's really just an 'append only' writing.

Thus, we invent the `State` monad!

## Implementation

```haskell
-- Allow re-declaration of instance signatures to help me debug.
{-# Language InstanceSigs #-}

import Data.Function ((&))

-- Basically, the `State` is 'unmaterialized' in the sense that only
-- when you feed in the initial state can you then run things forward
-- to get the current value `a` and also the current underlying
-- context `s`.
--
-- I wish maybe that this was called StateTransformation. But oh well.
--
-- Why not simply write `runState :: (a, s)`, which is what `Writer`
-- did? Because `>>=` is only going to pass the prior value, not the
-- prior state.
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  -- Notice how `fmap` won't make any change to the current state.
  fmap :: (a -> b) -> (State s a) -> (State s b)
  f `fmap` currentState = State $ \initialStateVal -> (
    let (currentVal, currentStateVal) = runState currentState initialStateVal
        nextVal = f currentVal
    in
      (nextVal, currentStateVal)
    )

instance Applicative (State s) where
  pure :: a -> (State s a)
  pure x = State $ \s -> (x, s)

  -- TODO: I'm too dumb to figure out what <*> means in this case?
  --
  -- It looks like Haskell lets you off with a warning for unimplemented
  -- methods...

instance Monad (State s) where
  return :: a -> (State s a)
  return = pure

  -- >>= sequences one state transformation after the other.
  (>>=) :: (State s a) -> (a -> State s b) -> State s b
  currentState >>= f = State $ \initialStateVal -> (
    let (currentVal, currentStateVal) = runState currentState initialStateVal
        (nextVal, nextStateVal) = runState (f currentVal) currentStateVal
    in
      (nextVal, nextStateVal)
    )
```

## Stack Example

```haskell
type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = State $ \xs -> ((), x:xs)

manipulateStack :: State Stack ()
manipulateStack = do
  x <- pop
  push (x * 2)
  push (x * 3)
  push (x * 4)

main = do
  putStrLn (stack & show)
  where
    (_, stack) = runState manipulateStack [10]
```

## Naming

The name `State` is a bit of a misnomer. The value `manipulateStack` is
a good example. It looks more like a value rather than a function. It is
sort-of hiding that the `State` returned by `manipulateStack` actually
needs to have a `stateValue` injected into it in order to be run. Only
then can a resulting value and state value be returned.

Perhaps a better name would have been something like
`StateTransformation`. A `State` is describing *what to do*, not a
specific state at any moment in time.

Related to this is how the `push` operations in the `do` syntax have a
side-effect, despite not producing a value that is bound to any explicit
variable. This is because the monad's context value (which is the state
transformation function) *is* changing. It's just that this change of
context is being hidden from us.

## Helper Functions

There are some helpers defined in `Control.Monad.State`:

```haskell
-- Pulls the current state up into the value of the monad.
get :: State stateType stateType
get = \stateVal -> (stateVal, stateVal)

-- Sets the state.
put :: newStateType -> State () oldStateType
put newStateVal = \oldStateVal -> ((), newStateVal)

-- Applies a function that modifies the state.
modify :: (s -> s) -> State s ()
modify f = state $ \ s -> ((), f s)
```

Similar to `Writer`, we have `runState`, `execState`, and `evalState`. I
think they all strictly evaluate state?

## Random Number Generation Example

```haskell
-- You may have to install random package. `stack install random`
import qualified System.Random as R
import Control.Monad.State (runState, State, state)

-- This is the signature of the random function.
--
-- random :: (RandomGen g, Random a) => g -> (a, g)
--
-- Basically, it takes a `RandomGen`, gets the next random bytes,
-- deserializes a new instance of `a`, and then returns the value with
-- the new `RandomGen` state.

-- Remember that `getNextRandom`, despite being a `State g a`, means
-- that it is a description of how to take the old state, and produce
-- a new state along with the value of type `a`. So `getNextRandom` is
-- kind of a function even though it looks like a constant.
getNextRandom :: (R.RandomGen g, R.Random a) => State g a
-- Note that I use the `state` function to create a `State`
-- value. This has been the way to produce a `State` ever since monad
-- transformers were introduced. LYAH is out-of-date in this respect.
--
-- Note that `state` is, of course, passed the function `R.random` that
-- takes in a state value and produces both a value and a state value.
-- That's exactly what a `State` is: a kind of function.
getNextRandom = state R.random

-- Again, note that this is a `State` value. Again, it is a
-- *description* of how to produce the new state and three bools.
getThreeRandomBools :: State R.StdGen (Bool, Bool, Bool)
getThreeRandomBools = do
  bool1 <- getNextRandom
  bool2 <- getNextRandom
  bool3 <- getNextRandom
  return (bool1, bool2, bool3)

main = do
  putStrLn (show boolsRoundOne)
  putStrLn (show boolsRoundTwo)
  where
    initialStdGen = (R.mkStdGen 0)
    ((boolsRoundOne, boolsRoundTwo), _) = runState (
      do
        boolsRoundOne <- getThreeRandomBools
        boolsRoundTwo <- getThreeRandomBools
        return (boolsRoundOne, boolsRoundTwo)
      ) initialStdGen
```

* Source
  * The `State` monad is defined here:
  * https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-221
