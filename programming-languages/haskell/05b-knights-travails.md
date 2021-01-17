# List Comprehensions and Guards: Knight's Travails

The LYAH people are excited to tell us that list comprehensions are just
syntactic sugar for `do` syntax. We've seen above how nested
comprehensions could clearly be written as a sequence of actions of the
list monad.

The LYAH people also want to show us how *guards* work. Remember how
this is filtered?

```haskell
-- Only return those numbers with a 7 in the base 10 representation.
[ x | x <- [1..50], '7' `elem` show x ]
```

This functionality is provided by the `guard` function. I'll use it,
then explain it in the next reading. I'll show an example of Knight's
Travails.

```haskell
import Data.Function ((&))
import Control.Monad (guard)

-- A position on the chess board
type Position = (Int, Int)

-- Reject positions off the chess board.
isValidPosition :: Position -> Bool
isValidPosition (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

-- Generate all possible positions on the chess board. Will use do
-- syntax.
nextPositions :: [Position] -> [Position]
nextPositions currentPositions = do
  -- Bind a currentPosition
  (x, y) <- currentPositions
  -- Bind a nextPosition. But it might be invalid!
  nextPosition <- [(x - 2, y - 1), (x - 2, y + 1),
                   (x - 1, y - 2), (x - 1, y + 2),
                   (x + 1, y - 2), (x + 1, y + 2),
                   (x + 2, y - 1), (x + 2, y +1)]
  -- guard will return `[]` if `isValidPosition` returns False. Else it
  -- returns `[()]`. It's exactly what we need to eliminate paths off
  -- the board, and keep exactly one copy of the path on the board.
  guard (isValidPosition nextPosition)
  return nextPosition

-- This generates all positions that can be reached in maxNumSteps
-- (or fewer).
allReachablePositions_ :: Int -> [Position] -> [[Position]]
allReachablePositions_ 0 currentPositions = [currentPositions]
allReachablePositions_ maxNumSteps currentPositions =
  positionsAfterLessThanNSteps ++ [positionsAtNSteps]
  where
    positionsAfterLessThanNSteps =
      allReachablePositions_ (maxNumSteps - 1) currentPositions
    positionsAtNMinusOneSteps = last positionsAfterLessThanNSteps
    positionsAtNSteps = nextPositions positionsAtNMinusOneSteps

-- Checks if a path of length at most `maxNumSteps` exists between two
-- positions.
isReachable :: Int -> Position -> Position -> Bool
isReachable maxNumSteps startPos endPos =
  endPos `elem` concat (allReachablePositions_ maxNumSteps [startPos])

main = do
  putStrLn ((isReachable 6 (1, 1) (8, 8)) & show)
```
