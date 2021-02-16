-- 標準の型クラス

-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a

-- class Eq a => Ord a where -- Ordを実装する型はEqも実装している必要がある
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- class Bounded a where
--   minBound :: a
--   maxBound :: a

-- class Show a where
--   show :: a -> String


-- 型クラスの派生
-- derivingでHaskellに型クラスの実装を任せることもできる
data IceCream = Chocolate | Vanilla　deriving (Show, Eq, Ord)

inc :: Int -> Int
inc x = x + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
  then minBound
  else succ n