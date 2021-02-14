-- レキシカルスコープ
x = 4
add1 y = y + x -- add1 1 is 5
add2 y = (\x -> y + x) 3 -- add2 1 is 4
add3 y = (\y -> (\x -> y + x) 1) 2 -- add3 1 is 3

-- lesson
simple = (\x -> x)

makeCharge = (\owed given ->
  if given > owed
  then given - owed
  else 0)

inc = (\x -> x + 1)

double = (\x -> x * 2)

square = (\x -> x ^ 2)