printDouble :: Int -> String
printDouble n = show(n * 2)

-- lesson myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x
  then x:myFilter f xs
  else myFilter f xs

-- lesson myHead & myTail
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

-- lesson 
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs