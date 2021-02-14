-- リスト

-- 先頭要素のhead, それ以外の要素のtail, 末端の[]で構成される
headItem = head [1, 2, 3]
tailItem = tail [1, 2, 3]

--コンス(:) コンシング
sample01 = 1:2:3:4:5:[]
sample02 = (1,2):(3,4):(5,6):[]
sample03 = 1:[]
sample04 = 1:[2,3,4,5]
sample05 = ['h', 'e', 'l', 'l', 'o']
sample06 = 'h':'e':'l':'l':'o':[]
sample07 = 'h':"ello"
-- error = "h": "ello"
sample08 = "he" ++ "llo"
sample09 = [1 .. 10]
sample10 = [1,3 .. 10]
sample11 = [1,1.5 .. 5]
sample12 = [1,0 .. -10]

-- インデックスアクセス(!!)
index01 = [1, 2, 3] !! 0
index02 = "puppies" !! 4
index03 = [1 .. 10] !! 11 -- exception
index04 = (!!) [1, 2, 3] 1

-- length
length01 = length [1 .. 20]
length02 = length [(10, 20), (1, 2), (15, 16)]
length03 = length "quicksand"

-- reverse
reverse01 = reverse [1, 2, 3]
isPalindrome word = word == reverse word -- 回文チェック

-- elem
elem01 = elem 13 [1 .. 50]
elem02 = elem 'p' "cheese"

respond phrase = if '!' `elem` phrase
  then "wow!"
  else "uh..okay"

-- take
take01 = take 5 [2, 4 .. 100]
take02 = take 3 "wonderful"
take03 = take 10000 [1, 2]

takeLast n aList = reverse(take n (reverse aList))
take04 = takeLast 10 [1 .. 100]

--zip 組み合わせてタプルのペアを作る
zip01 = zip [1, 2, 3] [2, 4, 6]
zip02 = zip "dog" "rabbit"
zip03 = zip ['a' .. 'f'] [1 ..]

-- cycle 無限の長さのリストを作成する
ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
  where groups = cycle [1 .. n]

assign01 = assignToGroups 3 ["Bob", "Kathy", "Sue", "Joan", "Jim", "Mike"]

-- 前置表記を用いた部分適用
paExample1 = (!!) "dog"
paExample2 = ("dog" !!) -- 2項演算子で部分適用をするにはカッコで囲む(セクション)
paExample3 = (!! 2) -- 左オペランドを待機、3文字目を返す

-- 遅延評価
longList = [1 ..]
stillLongList = (\x -> x) longList