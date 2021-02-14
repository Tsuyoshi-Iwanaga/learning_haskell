-- whereで関数内での変数を実現している関数
sumSquareOrSquareSum x y = if sumSquare > squareSum 
  then sumSquare
  else squareSum
  where
    sumSquare = x^2 + y^2
    squareSum = (x+y) ^ 2

-- まずは関数を分割してみる
sumSquareOrSquareSum02 x y = body (x^2 + y^2) ((x+y) ^ 2)

body sumSquare squareSum = if sumSquare > squareSum
  then sumSquare
  else squareSum

-- 02のbody関数をラムダ関数に書き換えてみる
sumSquareOrSquareSum03 x y = (\sumSquare squareSum -> if sumSquare > squareSum
  then sumSquare
  else squareSum) (x^2 + y^2) ((x+y) ^ 2)

-- let式と組み合わせてみる 先に変数を宣言する
sumSquareOrSquareSum04 x y = let
  sumSquare = (x^2 + y^2)
  squareSum = (x + y) ^ 2
  in
    if sumSquare > squareSum
      then sumSquare
      else squareSum