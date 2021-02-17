data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

-- SixSidedDieの型に対してShowのインスタンスを作成する
instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- 複雑な型のための型クラス
data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [
  Name ("Emil", "Cioran"),
  Name ("Eugene", "Thacker"),
  Name ("Friedrich", "Nietzsche")]

-- sort names ちゃんとlastNameでソートされる

-- lesson1
data Number = One | Two | Three deriving Enum

instance Eq Number where
  (==) num1 num2 = (fromEnum num1) == (fromEnum num2)

instance Ord Number where
  compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

-- lesson2
data FiveSideDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSideDie where
  roll n = toEnum (n `mod` 5)