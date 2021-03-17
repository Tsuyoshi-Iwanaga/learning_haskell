import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) []

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

updatedBiB2 :: UArray Int Int
updatedBiB2 = accum (*) updatedBiB $ zip [0 .. 3] $ cycle [3]

-- STUArray
listToArray :: [Int] -> UArray Int Int
listToArray vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- バブルソート
myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - 1)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray