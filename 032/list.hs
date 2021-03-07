import Control.Monad
import Data.Char

-- リストモナド
powerOfTwo :: Int -> [Int]
powerOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powerOfTwoAndThree :: Int -> [(Int, Int)]
powerOfTwoAndThree n = do
  value <- [1 .. n]
  let powerOfTwo = (2 ^ value)
  let powerOfThree = (3 ^ value)
  return (powerOfTwo, powerOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

-- guard
evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value) --条件を満たさない値を全て除外
  return value

-- リスト内包
powerOfTwoInnerList :: Int -> [Int]
powerOfTwoInnerList n = [2 ^ value | value <- [1 .. n]]

powerOfTwoAndThreeInnerList :: Int -> [(Int, Int)]
powerOfTwoAndThreeInnerList n = [(powerOfTwo, powerOfThree)
  | value <- [1 .. n]
  , let powerOfTwo = 2 ^ value
  , let powerOfThree = 3 ^ value]

allEvenOddsInnerList :: Int -> [(Int, Int)]
allEvenOddsInnerList n = [(evenValue, oddValue) | evenValue <- [2, 4 .. n], oddValue <- [1, 3 .. n]]

evensGuardInnerList :: Int -> [Int]
evensGuardInnerList n = [value | value <- [1 .. n], even value]