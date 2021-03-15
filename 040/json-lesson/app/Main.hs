module Main where
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

main :: IO ()
main = print "hi"

data Book = Book {
  title :: T.Text
  , author :: T.Text
  , year :: Int
} deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {
  author = "Will Cart"
  , title = "Learn Haskell"
  , year = 2017
}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

-- JSONを変換する
rawJSON :: BC.ByteString
rawJSON = "{\"author\": \"Emil Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

-- JSON(形式に誤りアリ)を変換する Nothingが返る
wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\": \"Emil Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON


instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

data ErrorMessage = ErrorMessage {
  message :: T.Text
  , error :: Int
} deriving Show

sampleError :: BC.ByteString
sampleError = "{\"message\": \"oops!\", \"error\": 123}"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object [
    "message" .= message
    , "error" .= errorCode ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0


-- NOAAのデータを扱ってみる

-- NOAAResult
data NOAAResult = NOAAResult {
  uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Double
  , resultId :: T.Text } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) = NOAAResult
    <$> v .: "uid"
    <*> v .: "mindate"
    <*> v .: "maxdate"
    <*> v .: "name"
    <*> v .: "datacoverage" -- resultIdをidに紐付けるためカスタムインスタンスにする
    <*> v .: "id"

-- Resultset
data Resultset = Resultset {
  offset :: Int
  , count :: Int
  , limit :: Int
} deriving (Show, Generic)

instance FromJSON Resultset

-- Metadata
data Metadata = Metadata {
  resultset :: Resultset
} deriving (Show, Generic)

instance FromJSON Metadata

-- NOAAResponse
data NOAAResponse = NOAAResponse {
  metadata :: Metadata
  , results :: [NOAAResult]
} deriving (Show, Generic)

instance FromJSON NOAAResponse

--IO
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results $ \result -> do
    let dataName = name result
    print dataName

noaaMain :: IO ()
noaaMain = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults