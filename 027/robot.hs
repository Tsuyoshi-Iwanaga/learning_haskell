import qualified Data.Map as Map

-- Parts
data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
  name = "left arm",
  description = "left arm for face punching",
  cost = 1000.0,
  count = 3}

rightArm :: RobotPart
rightArm = RobotPart {
  name = "right arm",
  description = "right arm for kind hand gestures",
  cost = 1025.0,
  count = 5}

robotHead :: RobotPart
robotHead = RobotPart {
  name = "robot head",
  description = "this head looks mad",
  cost = 5092.25,
  count = 2}

-- HTMLテキスト
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
  "<h2>", partName, "</h2>",
  "<p><h3>desc</h3>", partDesc, "</p>",
  "<p><h3>cost</h3>", partCost, "</p>",
  "<p><h3>count</h3>", partCount, "</p>"
  ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

-- RobotPartのデータベース
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- アイテムを1つ取得(Maybe RobotPartなのでMaybe Htmlに変換する)
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB --leftArm

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal --Functorのfmapで変換

-- パーツのリストをHTMLに変換
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts -- Listにおける<$>は単なるmap

-- DBを直接HTMLに変換する
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- IO
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


-- lesson
data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box val) = Box (func val) 

morePresents :: Int -> Box a -> Box [a]
morePresents count present = fmap (nCopies count) present
  where
    nCopies count item = (take count . repeat) item