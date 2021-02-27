import qualified Data.Map as Map

data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
  name = "left arm",
  description = "left arm for face punching!",
  cost = 1000.0,
  count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
  name = "right arm",
  description = "right arm for kind hand gestures!",
  cost = 1025.0,
  count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
  name = "robot head",
  description = "this head looks mad",
  cost = 5092.25,
  count = 2
}

--HTML
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

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- Maybe HTMLに変換
-- insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- HTMLのリストに変換
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- IO
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO