data User = User {
  name :: String,
  gemeId :: Int,
  score :: Int
} deriving Show

-- 値が欠損する可能性がある
serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- 欠損する可能性があるMaybe型でデータを生成
user1 = User <$> serverUsername <*> serverGamerId <*> serverScore

-- ユーザの入力を数値のIO型へ変換する
readInt :: IO Int
readInt = read <$> getLine

-- 入力からユーザを作成する
main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user