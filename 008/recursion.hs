--length
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--take
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:(myTake (n-1) xs)

--cycle
myCycle (first:rest) = first:(myCycle (rest ++ [first]))

--reverse
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

--ackermann
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

--collatz
collatz 1 = 1
collatz n = if even n
  then 1 + collatz (n `div` 2)
  else 1 + collatz (n * 3 + 1)

-- フィボナッチ数列
fib _ _ 0 = 0
fib _ _ 1 = 1
fib _ _ 2 = 1
fib x y 3 = x + y
fib x y counter = fib (x + y) x (counter - 1)


