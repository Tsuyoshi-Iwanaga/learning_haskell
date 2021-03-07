import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [
  (1, "nYarlathoTep"),
  (2, "KINGinYELLO"),
  (3, "dagon1997"),
  (4, "rcarter1919"),
  (5, "xCTHULHUx"),
  (6, "yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [
  ("nYarlathoTep", 2000),
  ("KINGinYELLO", 15000),
  ("dagon1997", 300),
  ("rcarter1919", 12),
  ("xCTHULHUx", 50000),
  ("yogSOThoth", 150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits name = Map.lookup name creditsDB

-- 接続用関数(Monadを使わない場合)
-- altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
-- altLookupCredits Nothing = Nothing
-- altLookupCredits (Just username) = lookupCredits username

-- IDからクレジットを取得
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

-- もしもう1階層増えたら...
type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [
  (1001, 1),
  (1002, 2),
  (1003, 3),
  (1004, 4),
  (1005, 5),
  (1006, 6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

--IOアクションへの対応
echo :: IO ()
echo = getLine >>= putStrLn