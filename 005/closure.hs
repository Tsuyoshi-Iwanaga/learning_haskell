-- ラムダ関数を返す関数、引数fはラムダ関数からキャプチャされる、これがいわゆるクロージャ
genIfEven f = (\x -> ifEven f x)

ifEven f x = if even x
  then f x
  else x

-- リクエストURLを作成する関数
getRequestUrl host apikey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apikey

genHostRequestBuilder host = (\apikey resource id -> getRequestUrl host apikey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

-- 部分適用
add4items a b c d = a + b + c + d
anotherMystery = add4items 2 3
-- anotherMystery 4 5 で実行すると14が返ってくる