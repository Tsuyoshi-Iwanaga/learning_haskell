import Data.List -- sortを利用する
import Data.Semigroup

-- 関数合成
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

--myAny even [1,2,3]

-- Semigroup
instance Semigroup Integer where
  (<>) x y = x + y -- <>演算子を単純な加算として定義

-- Color Semigroup
data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b | a == b = a
          | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
          | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
          | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
          | otherwise = Brown

-- Red <> Yellow
-- Green <> (Blue <> Yellow)


-- Monoid 単位元による合成
-- 単位元 : 整数なら0, リストなら[]
-- 畳み込み関数を使えるようになる

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map(\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

-- createPTable ["head", "tails"] [0.5, 0.5]

-- 直積(デカルト積)を求める
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1　--l1を写像し、要素のコピーをnToAdd個作成
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2 -- l2を無限リストに、zipWithで2つのリストを結合

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- PTableをSemigroupのインスタンスに
instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

-- PTableをMonoidのインスタンスに
instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

-- インスタンス生成
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

-- coin <> spinner
-- mconcat [coin, coin, coin]