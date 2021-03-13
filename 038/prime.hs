maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs <*> myTakeSafer (n-1) (Just (tail xs)) 

-- 素数か判定する関数(Maybeだと-10や100など原因が異なるエラーも全てNothingで処理されてしまう)
primes :: [Int]
primes = [2, 3, 5, 7]

maxX :: Int
maxX = 10

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n > maxX = Nothing
  | otherwise = Just (n `elem` primes)

-- Either
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

-- Eitherの素数チェッカー
data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

isPrimeEither :: Int -> Either PrimeError Bool
isPrimeEither n
  | n < 2 = Left InvalidValue
  | n > maxX = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrimeEither n
  print (displayResult result)