-- 特定のNまでの素数を探す
hoge = pure (*) <*> [2 .. 4] <*> [2 .. 4]

primeToN :: Integer -> [Integer]
primeToN n = filter isNotCompose twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotCompose = not . (`elem` composite)