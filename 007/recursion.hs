-- 再帰で最大公約数を見つける
myGCD a b = if remainder == 0
  then b
  else myGCD b remainder
  where remainder = a `mod` b

-- パターンマッチで記述した場合
myGCD2 a 0 = a
myGCD2 a b = myGCD2 b (a `mod` b)