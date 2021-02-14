-- calcChange owed given = if given - owed > 0
--   then given - owed
--   else 0

-- where句を使う
calcChange owed given = if change > 0
  then change
  else 0
  where
    change = given - owed

-- lesson
doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

inc n = n + 1

double n = n * 2

square n = n ^ 2

ifEven n = if even n
  then n - 2
  else 3 * n + 1