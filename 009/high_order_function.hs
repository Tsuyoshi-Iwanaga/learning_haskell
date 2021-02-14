import Data.Char

--map
map01 = map reverse ["dog", "cat", "moose"]
map02 = map head ["dog", "cat", "moose"]
map03 = map (take 4) ["pumpkin", "pie", "peanut butter"]

--再帰でmapを実装する
addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

map04 = addAnA ["dog", "cat", "moose"]

--再帰でmapを実装する2
squareAll [] = []
squareAll (x:xs) = (x^2):squareAll xs

--mapを自前実装
myMap _ [] = []
myMap f (x:xs) = (f x):myMap f xs


--filter
fil01 = filter even [1 .. 4]
fil02 = filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]

myFilter _ [] = []
myFilter f (x:xs) = if f x
  then x:myFilter f xs
  else myFilter f xs


--foldl
foldl01 = foldl (+) 0 [1..4]

myProduct aList = foldl (*) 1 aList

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

--foldr
myFoldr f init [] = init
myFoldr f init (x:xs) = f x (myFoldr f init xs)


--lesson

--elem
myElem n aList = length filteredList > 0
  where filteredList = filter (\x -> x == n) aList

--palindrome
isPalindrome text = processedText == reverse processedText
  where
    noSpaces = filter (\x -> x /= ' ') text
    processedText = map toLower noSpaces

-- harmonic
harmonic n = foldl (+) 0 (map (\x -> 1 / x) [1..n])

harmonic2 n = sum (take n seriesValues)
  where
    seriesPairs = zip (cycle [1.0]) [1.0, 2.0 ..]
    seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs