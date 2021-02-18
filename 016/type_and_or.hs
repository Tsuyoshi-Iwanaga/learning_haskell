-- 直積型
data AuthorName = AuthorName String String

data Book = Book AuthorName String Int Double

data AuthorName_l = AuthorName_l {
  first_name :: String,
  last_name :: String
}

data Book_l = Book_l {
  author :: AuthorName,
  isbn :: String,
  title :: String,
  year :: Int,
  price :: Double
}

-- 直和型
data Name = Name String String
  | NameWithMiddle String String String
  | TwoInitialsWithLast Char Char String
  | FirstNameWithTwoInits String Char Char

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name
data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

-- ブックストアプログラム
data Book_a = Book_a {
  bookAuthor :: Creator,
  bookIsbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double
}

data VinyRecord = VinyRecord {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
  name :: String,
  description :: String,
  toyPrice :: Double
}

data Pamphlet = Pamphlet {
  pamphTitle :: String,
  pamphDescription :: String,
  pamphInfo :: String
}

data StoreItem = BookItem Book_a | RecordItem VinyRecord | ToyItem CollectibleToy | PamphItem Pamphlet

getPrice :: StoreItem -> Double
getPrice (BookItem book) = bookPrice book
getPrice (RecordItem record) = recordPrice record
getPrice (ToyItem toy) = toyPrice toy
getPrice (PamphItem _) = 0.0

madeBy :: StoreItem -> String
-- madeBy (BookItem book) = show (bookAuthor book)
-- madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"


-- Lesson
type Radius = Double
type Width = Double
type Height = Double

data Shape = Circle Radius | Square Height | Rectangle Width Height deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * r * pi
perimeter (Square h) = 4 * h
perimeter (Rectangle w h) = 2 * (w + h)

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square h) = h ^ 2
area (Rectangle w h) = w * h

-- instance
shapeCircle :: Shape
shapeCircle = Circle 5.0

shapeSquare :: Shape
shapeSquare = Square 5.0