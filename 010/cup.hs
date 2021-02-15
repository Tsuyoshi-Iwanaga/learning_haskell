-- コンストラクタ 
cup amount = \message -> message amount

-- インスタンス生成
coffeeCup = cup 12

-- ゲッター(メッセージ)
getAmount aCup = aCup (\x -> x)

-- プロパティの値を変更した新しいオブジェクトを返す(メッセージ)
drink aCup amount = if remained >= 0
  then cup remained
  else cup 0
  where
    remained = (getAmount aCup) - amount

-- カップが空かを判定する(メッセージ)
isEmpty aCup = getAmount aCup == 0

-- カップを連続ですする(メッセージ)
afterManySips aCup aList = foldl drink aCup aList