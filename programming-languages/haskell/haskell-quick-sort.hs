-- Makes it a little easier to destructure a record.
{-# LANGUAGE NamedFieldPuns #-}

import qualified Control.Monad
import qualified Control.Monad.ST
import qualified Data.Array.Base
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX
import Prelude hiding (length)
-- You need to install mwc-random
import qualified System.Random.MWC as Random.MWC

type UArray = Data.Array.Base.UArray Int Int
type STUArray s = Data.Array.Base.STUArray s Int Int

-- ArraySlice data type. Note the bogus `s` parameter for use in `ST`.
data ArraySlice s = ArraySlice {
  backingArray :: STUArray s,
  startIdx :: Int,
  length :: Int
}

-- Read a location in the ArraySlice
readArraySlice :: ArraySlice s -> Int -> Control.Monad.ST.ST s Int
readArraySlice arraySlice idx =
  -- Doing unsafe reads/writes is very important for performance!
  Data.Array.Base.unsafeRead backingArray (startIdx + idx)
  where
    ArraySlice { backingArray, startIdx, length } = arraySlice

-- Write a location in the ArraySlice.
writeArraySlice :: ArraySlice s -> Int -> Int -> Control.Monad.ST.ST s ()
writeArraySlice arraySlice idx val =
  -- Doing unsafe reads/writes is very important for performance!
  Data.Array.Base.unsafeWrite backingArray (startIdx + idx) val
  where
    ArraySlice { backingArray, startIdx, length } = arraySlice

-- Move the startIdx and change the length.
resliceArraySlice :: ArraySlice s -> Int -> Int -> ArraySlice s
resliceArraySlice arraySlice newStartIdx newLength =
  ArraySlice {
    backingArray = backingArray,
    startIdx = startIdx + newStartIdx,
    length = newLength
  }
  where
    ArraySlice { backingArray, startIdx, length } = arraySlice

-- Moves all items smaller than the pivot to the left of the
-- pivot. Everything greater is to the right of the pivot.
partition :: ArraySlice s -> Int -> Int -> Control.Monad.ST.ST s Int
partition arraySlice pivotIdx idx
  | idx == (length arraySlice) = return pivotIdx
  | otherwise = do
    pivot <- readArraySlice arraySlice pivotIdx
    el <- readArraySlice arraySlice idx
    if el < pivot
      then do
        let nextIdx = pivotIdx + 1
        nextEl <- readArraySlice arraySlice nextIdx
        -- Perform the three-fold exchange.
        writeArraySlice arraySlice idx nextEl
        writeArraySlice arraySlice nextIdx pivot
        writeArraySlice arraySlice pivotIdx el
        partition arraySlice (pivotIdx + 1) (idx + 1)
      else do
        partition arraySlice pivotIdx (idx + 1)

-- QuickSort runs the partition subroutine to place the pivot, and then
-- sorts the left and right halves recursively.
--
-- The whole point of quick sort is to do it in place. But most
-- introductions to Haskell will show you how to do it in an immutable
-- (and extremely inefficient) way.
qsort :: ArraySlice s -> Control.Monad.ST.ST s ()
qsort arraySlice
  | (length arraySlice) == 0 = return ()
  | otherwise = do
      pivotIdx <- partition arraySlice 0 0
      let leftLength = pivotIdx
      qsort (resliceArraySlice arraySlice 0 leftLength)
      let rightLength = (length arraySlice) - (pivotIdx + 1)
      qsort (resliceArraySlice arraySlice (pivotIdx + 1) rightLength)

-- A function to benchmark how long an operation takes.
benchmark :: IO a -> IO (Clock.NominalDiffTime, a)
benchmark f = do
  t1 <- POSIX.posixSecondsToUTCTime `fmap` POSIX.getPOSIXTime
  result <- f
  t2 <- POSIX.posixSecondsToUTCTime `fmap` POSIX.getPOSIXTime
  let timeDiff = Clock.diffUTCTime t2 t1

  return (timeDiff, result)

-- Some constants
numElems :: Int
numElems = 10 * 1000 * 1000
maxValue :: Int
maxValue = 10 * 1000 * 1000

-- Generate a UArray of unsorted elements.
generateUnsortedElements :: IO UArray
generateUnsortedElements = do
    generator <- Random.MWC.create
    unsortedElements <- (
      Control.Monad.forM
        [0..numElems]
        (\_ -> Random.MWC.uniformR (0, maxValue) generator)
      )
    return (
      Data.Array.Base.listArray
        -- Ugh, bounds are *inclusive*.
        (0, numElems - 1)
        unsortedElements
      )

-- Sort the elements in a UArray. This will use `unsafeThaw`, which will
-- assume that no other thread is given the UArray. This allows
-- `sortElements` not to copy over the input into its own buffer.
sortElements:: UArray -> UArray
sortElements unsortedElements = Control.Monad.ST.runST $ do
  arr <- (Data.Array.Base.thaw unsortedElements) :: Control.Monad.ST.ST s (STUArray s)
  -- Ugh, bounds are *inclusive*.
  (_, numElems) <- Data.Array.Base.getBounds arr
  qsort (ArraySlice {
    backingArray = arr,
    startIdx = 0,
    length = numElems + 1
  })
  (Data.Array.Base.freeze arr)

main :: IO ()
main = do
  -- Generate random list of numbers. Apparently System.Random is very
  -- slow...
  -- http://hackage.haskell.org/package/mwc-random-0.14.0.0/docs/System-Random-MWC.html
  putStrLn "Generating random numbers!"
  (timeDiff, unsortedElements) <- benchmark generateUnsortedElements
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 1.02sec.

  -- Build the array for in place sorting.
  -- http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-IO.html
  putStrLn "Building/sorting array!"
  (timeDiff, sortedElements) <- benchmark $ do
    let sortedElements = sortElements unsortedElements
    sortedElements `seq` return sortedElements
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 1.15sec.

  putStrLn (show (sortedElements Data.Array.Base.! (numElems - 1)))
