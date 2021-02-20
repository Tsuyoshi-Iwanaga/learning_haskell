import qualified Data.Map as Map

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- Triple(同じ型を3つ保持する型)
data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String
aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char
initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- リスト
-- data [] a = [] | a:[a]

-- リストを自分で実装してみる
data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1 : 2 : 3 : []

outListEx1 :: List Int
outListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c' : 'a': 't' : []

outListEx2 :: List Char
outListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

outMap :: (a -> b) -> List a -> List b
outMap _ Empty = Empty
outMap func (Cons a rest) = Cons (func a) (outMap func rest)

-- 複数パラメータを持つ型

-- タプル
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- カインド
-- :kind *で型の型を表す

-- Data.Map
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 34]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs