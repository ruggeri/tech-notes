import Control.Monad (forM, liftM)
import Data.Array.IO (IOUArray, getElems, newListArray)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Function ((&))
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
-- You need to install mwc-random
import System.Random.MWC (create, uniformR)

-- TODO: I should be able to do this with the ST monad, but I'm not
-- smart enough for that yet.

-- ArraySlice data type. I probably should be able to parameterize
-- over the kind of MArray. Indeed, ArraySlice probably can implement
-- MArray itself!
--
-- http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-MArray.html
data ArraySlice = ArraySlice (IOUArray Int Int) Int Int

readArraySlice :: ArraySlice -> Int -> IO Int
readArraySlice as@(ArraySlice arr offset _) idx = do
  unsafeRead arr (fromIntegral (offset + idx) :: Int)

writeArraySlice :: ArraySlice -> Int -> Int -> IO ()
writeArraySlice as@(ArraySlice arr offset _) idx val = do
  unsafeWrite arr (fromIntegral (offset + idx) :: Int) val

resliceArraySlice :: ArraySlice -> Int -> Int -> ArraySlice
resliceArraySlice as@(ArraySlice arr offset _) newOffset newLen =
  ArraySlice arr (offset + newOffset) newLen

asLength :: ArraySlice -> Int
asLength as@(ArraySlice _ _ len) = len

-- Moves all items smaller than the pivot to the left of the
-- pivot. Everything greater is to the right of the pivot.
partition :: ArraySlice -> Int -> Int -> IO Int
partition as pivotIdx idx
  | idx == (asLength as) =
      return pivotIdx
  | otherwise = do
      pivot <- readArraySlice as pivotIdx
      el <- readArraySlice as idx
      if el < pivot
        then do
          let nextIdx = pivotIdx + 1
          nextEl <- readArraySlice as nextIdx
          writeArraySlice as idx nextEl
          writeArraySlice as nextIdx pivot
          writeArraySlice as pivotIdx el
          partition as (pivotIdx + 1) (idx + 1)
        else do
          partition as pivotIdx (idx + 1)

-- QuickSort runs the partition subroutine to place the pivot, and
-- then sorts the left and right halves recursively.
--
-- The whole point of this gist is to show how to do this *in
-- place*. The whole point of quick sort is to do it in place. But
-- most introductions to Haskell will show you how to do it in an
-- immutable (and extremely inefficient) way.
qsort :: ArraySlice -> IO ()
qsort as
  | (asLength as) == 0 = do
      return ()
  | otherwise = do
      pivotIdx <- partition as 0 0
      let leftLen = pivotIdx
      qsort (resliceArraySlice as 0 leftLen)
      let rightLen = (asLength as) - (pivotIdx + 1)
      qsort (resliceArraySlice as (pivotIdx + 1) rightLen)

-- A function to benchmark how long an operation takes.
benchmark :: IO a -> IO (NominalDiffTime, a)
benchmark f = do
  t1 <- getPOSIXTime & liftM posixSecondsToUTCTime
  result <- f
  t2 <- getPOSIXTime & liftM posixSecondsToUTCTime
  let timeDiff = diffUTCTime t2 t1

  return (timeDiff, result)

-- Some constants!
numElems :: Int
numElems = 10 * 1000 * 1000
maxValue :: Int
maxValue = 10 * 1000 * 1000

main :: IO ()
main = do
  -- Generate random list of numbers. Apparently System.Random is very slow...
  -- http://hackage.haskell.org/package/mwc-random-0.14.0.0/docs/System-Random-MWC.html
  putStrLn "Generating random numbers!"
  (timeDiff, elems) <- benchmark $ do
    gen <- create
    elems <- forM [1..numElems] $ \_ -> uniformR (0, maxValue) gen
    return elems
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 1.2sec!!

  -- Build the array for in place sorting.
  -- http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-IO.html
  putStrLn "Building array!"
  (timeDiff, arr) <- benchmark $ do
    arr <- (newListArray (0, numElems - 1) elems) :: IO (IOUArray Int Int)
    return arr
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 0.05sec.

  -- Sort them!
  putStrLn "Sorting array!"
  (timeDiff, _) <- benchmark $ do
    qsort (ArraySlice arr 0 numElems)
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 1.25s. <<<
  -- A C++ version takes ~0.77s. So this isn't so bad. 60% slower.

  -- Extract list and print!
  putStrLn "Extracting array!"
  (timeDiff, elems) <- benchmark $ do
    elems <- getElems arr
    return elems
  putStrLn $ "Time: " ++ (show timeDiff)
  -- About 0.8s.

  putStrLn (show (last elems))

  --forM_ elems $ \el -> do
  --  putStrLn (show el)

  putStrLn "DONE"
