module Primes where

-- エラトステネスのふるい
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime: sieve noFactor
    where noFactor = filter (not . (== 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000] -- 設定できる数に制限を設けておく

-- 負の数や10000までの素数の最大値を超える場合は欠損値とする
-- 素数はJust True/合成数はJust False
isPrime :: Int -> Maybe Bool
isPrime n
    | n < 2 = Nothing
    | n >= last primes = Nothing 
    | otherwise = Just (n `elem` primes)

-- 与えられた数を素因数分解する
unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) =
    if n `mod` next == 0
    then next:unsafePrimeFactors (n `div` next) (next:primes)
    else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
    | n < 2 = Nothing
    | n >= last primes = Nothing
    | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter (<= n) primes

