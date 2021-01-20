-- Allows us to add type annotations more places.
{-# LANGUAGE ScopedTypeVariables #-}
-- Will allow us to rebind the (>>=) syntax.
{-# LANGUAGE RebindableSyntax #-}

import Control.Arrow ((>>>))
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Ratio
-- Need to hide return/>>= so that we can use our own.
import Prelude hiding (return, (>>=))

-- The type used to represent the probability.
type ProbabilityType = Rational

-- Represents a set of outcomes, with their probabilities.
type ProbabilitySet elementType = Map.Map elementType ProbabilityType

-- From a single value, creates a set representing a 100% chance of
-- that value.
return :: Ord elementType => elementType -> ProbabilitySet elementType
return value = Map.singleton value 1

-- Adds a (value, probability) pair to the set.
incrementProbability :: Ord elementType
  => elementType
  -> ProbabilityType
  -> ProbabilitySet elementType
  -> ProbabilitySet elementType
incrementProbability value newProb probabilitySet =
  Map.alter updateFn value probabilitySet
  where
    updateFn Nothing = Just newProb
    updateFn (Just oldProb) = Just (oldProb + newProb)

-- Adds a set of (key, probabilities) to a set. Weights all additions by
-- a base probability rate.
incrementProbabilities :: Ord elementType
  => ProbabilityType
  -> ProbabilitySet elementType
  -> ProbabilitySet elementType
  -> ProbabilitySet elementType
incrementProbabilities setABaseProb setA setB =
  Map.foldlWithKey updateFn setB setA
  where
    updateFn
      (newSet :: ProbabilitySet elementType)
      (newKey :: elementType)
      (keyProb :: ProbabilityType)
      = incrementProbability newKey (setABaseProb * keyProb) newSet

-- Executes a probabilistic function on a probabilistic input.
--
-- Note that, sadly, `ProbabilitySet` cannot quite be a Functor or
-- Monad, since there is a type constraint on the `elementType`: it must
-- be `Ord` to be used as a key for `Data.Map`.
--
-- Even if we were to represent `ProbabilitySet` as `[(elementType,
-- ProbabilityType)]`, we would still need to restrict `elementType` to
-- `Eq`, and this would likewise torpedo `ProbabilitySet` becoming a
-- `Functor` or `Monad`.
--
-- I think a natural solution is to *not* make `ProbabilitySet` a monad.
-- Instead, if we wanted to write do-style code, we would work with
-- `State ProbabilitySet`. We'd simply name this `>>=` method something
-- like `applyProbabilisticFn`.
--
-- Alternatively, we could investigate ConstraintKinds. This lets you
-- implement a typeclass like `Functor`, but the implementer specifies
-- some constraints on the generality of their implementation.
(>>=) :: (Ord oldElType, Ord newElType)
  => ProbabilitySet oldElType
  -> (oldElType -> ProbabilitySet newElType)
  -> ProbabilitySet newElType
oldSet >>= probabilisticFn =
    Map.foldlWithKey
      updateFn
      (Map.empty :: ProbabilitySet newElType)
      oldSet
  where
    updateFn
      (newSet :: ProbabilitySet newElType)
      (oldKey :: oldElType)
      (oldKeyProb :: ProbabilityType)
      = incrementProbabilities oldKeyProb (probabilisticFn oldKey) newSet

-- **CLIENT CODE**

-- Count the number of heads that occurred in a series of coin flips.
countHeads :: [Bool] -> Int
countHeads coinResults = length (filter (==True) coinResults)

-- Flip a 50/50 coin once.
flipCoin :: ProbabilitySet Bool
flipCoin = Map.empty & (addTrueFlip >>> addFalseFlip)
  where
    addTrueFlip = incrementProbability True (1%2)
    addFalseFlip = incrementProbability False (1%2)

-- Flip a 50/50 coin multiple times.
flipCoins :: Int -> ProbabilitySet [Bool]
flipCoins 0 = return []
flipCoins numFlips = do
  previousCoinResults <- flipCoins (numFlips - 1)
  currentCoinResult <- flipCoin
  return (currentCoinResult:previousCoinResults)

-- Flip a 50/50 coin multiple times and count the number of heads.
countCoinFlips :: Int -> ProbabilitySet Int
countCoinFlips numFlips = do
  coinResults <- flipCoins numFlips
  return $ countHeads coinResults

numCoinFlips :: Int
numCoinFlips = 16

main = do
  putStrLn (show headCountProbabilities)
  where
    headCountProbabilities = countCoinFlips numCoinFlips
