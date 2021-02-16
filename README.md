# learning Haskell
## 1. 概要

### インストール

```shell
brew haskell
```

* Glasgow Haskell Compiler (GHC) コンパイラ
* GHCi 対話型のインタープリタ
* プロジェクトを管理するためのビルドツール(stack)

### コンパイル

```shell
stack ghc hello.hs
```

```haskell
-- hello.hs my first Haskell file
main = do
	print "Hello World!"
```

下記のようなファイルができる

* hello (実行ファイル Windowsならhello.exe)
* hello.o
* hello.hi

```shell
./hello
-- "Hello World"
```

コンパイルされた実行ファイルはmain(エントリーポイント)が含まれている必要がある

### インタプリタ(GHCi)

インタプリタを実行する

```shell
ghci
-- :q 閉じる
-- :l ファイルを読み込む
```

## 2. 関数

### Haskellでの関数

* 少なくとも一つ以上の引数を必要とする
* 必ず値を返す
* 同じ引数で呼び出された時は常に同じ値を返す(**参照透過性**)

```haskell
simple x = x

simple 2
-- 2

simple "dog"
-- dog
```

引数を持たなければ関数外の値にアクセスしているはず(**グローバル変数**)
また、値を返さないとしたらやはり関数外の値を変更しているはず(**副作用**)

Haskellには他言語にあるインクリメント演算子(++)や代入演算子もない(+=)

### Haskellでの変数

変数とはいってもHaskellでは再代入によって変数の値を変更することはできない

```haskell
x = 2
x = 3 -- コンパイル時にエラー
```

### where

where句を使って関数内で利用する変数を実現する

```haskell
calcChange owed given = if change > 0
  then change
  else 0
  where change = given - owed
```

## 3.ラムダ関数とレキシカルスコープ

### ラムダ関数

匿名関数や無名関数ともいう
ラムダ構文を使ってHaskellでは次のように定義する

```haskell
\x -> x
-- (\x -> x) "hello"
```

### レキシカルスコープ

関数が生成されると同時に新しいスコープが生成される
スコープ内で関数が変数を解決できないとき、一つ外側のスコープを探す...という動作を繰り返す

```haskell
x = 4
add1 y = y + x -- add1 1 is 5: xは外側の4、yは引数の1
add2 y = (\x -> y + x) 3 -- add2 1 is 4: xは引数の3、yは外側の引数の1
add3 y = (\y -> (\x -> y + x) 1) 2 -- add3 1 is 3: xは引数の1、yは引数の2
```

## 4.ファーストクラス関数(第一級関数)

### 別の関数を引数で受け取る

関数を値として扱い、引数として利用できる

```haskell
ifEven myFunc x = if even x
  then myFunc x
  else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n
```

### 返り値を別の関数として返す

関数を値として扱い、関数の引数として返すことができる

```haskell
addressletter name location = (getlocationfunction location) name

-- dispatch(戻り値として関数を返す)
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

-- ny
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

-- sf
sfOffice name = if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

-- reno
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name
```

## 5.クロージャと部分適用

### クロージャ

ラムダ関数作成時に、外側から渡される変数はキャプチャされる

```haskell
genIfEven f = (\x -> ifEven f x) -- fがキャプチャされる
```

### 部分適用

また、引数が足りずに関数がよばれた場合は、それを待機する関数が返される
この機能のおかげであまりクロージャを意識する場面は少ない
ただ、左から順番に適用されていくため、引数は汎用的なものから並べる必要がある

```haskell
add4 a b c d = a + b + c + d
anotherAdd = add4 1 2
anotherAdd 3 4 -- 10
```

### flip

引数を入れ替えた関数を返す
独自に実装するとこうなる

```haskell
flipBinaryArgs fn = (\x y -> fn  y x)
```

これは標準のflip関数を使って同じことができる

```haskell
diff x y = x - y
revDiff = flip diff
-- diff 5 3
-- diff 3 5
```

## 6.リスト

### リストとは

先頭要素(head)、残り要素(tail)、末端([])をもつ構造

```haskell
head [1, 2, 3] -- 1
tail [1, 2, 3] -- [2,3]
```

### コンス(:)

```haskell
sample01 = 1:2:3:4:5:[]
sample02 = (1,2):(3,4):(5,6):[]
sample03 = 1:[]
sample04 = 1:[2,3,4,5]
sample05 = ['h', 'e', 'l', 'l', 'o']
sample06 = 'h':'e':'l':'l':'o':[]
sample07 = 'h':"ello"
-- error = "h": "ello" これはエラーになる
sample08 = "he" ++ "llo"
sample09 = [1 .. 10]
sample10 = [1,3 .. 10]
sample11 = [1,1.5 .. 5]
sample12 = [1,0 .. -10]
```

### !! 演算子(インデックスアクセス)

``` haskell
[1, 2, 3] !! 0 -- 1
"puppies" !! 4 -- i
[1 .. 10] !! 11 -- エラー
(!!) [1, 2, 3] 0 -- 1
```

### length

```haskell
length01 = length [1 .. 20]
length02 = length [(10, 20), (1, 2), (15, 16)]
length03 = length "quicksand"
```

### reverse

```haskell
reverse01 = reverse [1, 2, 3]
isPalindrome word = word == reverse word -- 回文チェック
```

### elem

```haskell
elem01 = elem 13 [1 .. 50]
elem02 = elem 'p' "cheese"

respond phrase = if '!' `elem` phrase
  then "wow!"
  else "uh..okay"
```

### take

```haskell
take01 = take 5 [2, 4 .. 100]
take02 = take 3 "wonderful"
take03 = take 10000 [1, 2]

takeLast n aList = reverse(take n (reverse aList))
take04 = takeLast 10 [1 .. 100]
```

### zip

```haskell
zip01 = zip [1, 2, 3] [2, 4, 6]
zip02 = zip "dog" "rabbit"
zip03 = zip ['a' .. 'f'] [1 ..]
```

### cycle

```haskell
ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
  where groups = cycle [1 .. n]

assign01 = assignToGroups 3 ["Bob", "Kathy", "Sue", "Joan", "Jim", "Mike"]
```

### 前置を用いた部分適用

```haskell
paExample1 = (!!) "dog"
paExample2 = ("dog" !!) -- 2項演算子で部分適用をするにはカッコで囲む(セクション)
paExample3 = (!! 2) -- 左オペランドを待機、3文字目を返す
```

### 遅延評価

下記のような無限の長さのリストが含まれるコードでもコンパイルが可能
これはコードが必要になるまで評価されない**遅延評価**を行うため

```haskell
longList = [1 ..] -- 無限の長さのリストを定義
stillLongList = (\x -> x) longList
```

### 遅延評価のメリット/デメリット

メリット

* 必要にならなければ実行されないためパフォーマンス上のアドバンテージがある
* 無限の長さのリストなどの構造を利用することができる

デメリット

* パフォーマンス計測が難しくなるなど明白さに欠ける
* 評価されない関数を大量に構築できてしまう

## 7. 再帰とパターンマッチング

### 再帰の必要性

関数型では変数への再代入ができないため、カウンタを使うforループは使用できない
さらに制御構文にもfor, while, untilといったものがないのでイテレーションは全て再帰で解決する必要がある

### 再帰を考えるコツ

1. 「どうなったら再帰が終了するかを決める」※空のリスト
2. 「関数を終了条件で呼び出すと何が返ってくるか」※空のリストだと返り値は？
3. 「再帰が継続となる状態はどんな状態かを洗い出す」※リストの中に何か入っている
4. 「繰り返し」のプロセスを決定する ※リストのtailに対して再帰を実行する
5. 繰り返しの度に最終目標に近づいていくかをチェックする ※tailでだんだんリストは空に近づく

### 最大公約数を求める

```haskell
myGCD a b = if remainder == 0
	then b
	else myGCD b remainder
  where remainder = a `mod` b
```

### パターンマッチング

引数で渡された値を照合し、その結果に応じて振る舞いを変えることができる機能
case文でこういう関数が記述されている場合...

```haskell
sayAmount n = case n of
	1 -> "one"
	2 -> "two"
	n -> "a bunch"
```

パターンマッチングを使うとこうなる

```haskell
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"
```

例：リストが空か判定する(使用しないパラメータは_にするのが慣習)

```haskell
isEmpty [] = True
isEmpty _ = False
```

例：リストはxとxsを使ってこのようにパターンマッチングを使うこともできる

```haskell
myHead (x:xs) = x
myHead [] = error "No head for empty list"
```

## 8.再帰の具体例

### リストで再帰を使う

リストで使用できる関数をいくつか再帰を使って実装してみる

#### length

```haskell
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
```

#### take

```haskell
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:(myTake (n-1) xs)
```

#### cycle

```haskell
myCycle (first:rest) = first:(myCycle (rest ++ [first]))
```

#### reverse

```haskell
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]
```

### 再帰ルールに従っているのに危険な例(アッカーマン関数)

```haskell
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
```

再帰は自身の中で自身を呼び出す構造のため計算量が爆発しやすい

### 再帰ルールから逸脱しているが実用的には問題ない例(コラッツ予想)

```haskell
collatz 1 = 1
collatz n = if even n
  then 1 + collatz (n `div` 2)
  else 1 + collatz (n * 3 + 1)
```

奇数の時の終了条件が一見すると終了条件に近づいて行っていないように見える
ただしこれは1に近づいていき必ず終了すると推定されている

## 9.高階関数

引数に関数を受け取る関数のこと
再帰を単純化する用途で使われることも多い

### map

```haskell
map01 = map reverse ["dog", "cat", "moose"]
map02 = map head ["dog", "cat", "moose"]
map03 = map (take 4) ["pumpkin", "pie", "peanut butter"]
```

### filter

```haskell
fil01 = filter even [1 .. 4]
fil02 = filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
```

### foldl

```haskell
foldl01 = foldl (+) 0 [1 .. 4]
```

いくつかfoldlを利用した畳み込みの例

```haskell
-- 総乗
myProduct aList = foldl (*) 1 aList

-- 文字の連結
concatAll xs = foldl (++) "" xs

-- 累乗して足し合わせる
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

-- 順番を逆にする
rcons x y = y:x
myReverse xs = foldl rcons [] xs
```

### foldr

右畳み込み、foldlと比較すると減算のように順序が重要な場合に差が出てくる
また無限の長さを持つリストでうまく動作する唯一の畳み込み

```haskell
foldl (-) 0 [1 .. 4] -- 0-1-2-3-4よって-10
foldr (-) 0 [1 .. 4] -- 4-3-2-1-0よって-2
```

## 10.関数型を用いたオブジェクト指向

### 単純な例

関数型におけるオブジェクトとは**値がキャプチャされた状態のラムダ関数**(つまり**クロージャ**)
これに対し、第一級関数として関数を渡すとオブジェクト内部でそれがキャプチャされた値に適用される仕組み
このようにして外から渡される関数を**メッセージ**とよび、メッセージをオブジェクトに送信するなどと言う

```haskell
-- コンストラクタ
cup amount = \message -> message amount
```

```haskell
-- インスタンス化
coffeeCup = cup 12
```

```haskell
-- ゲッターとなるメッセージ
getAmount aCup = aCup (\x -> x)
```

```haskell
-- 実行
getAmount coffeeCup -- 12
```

### 複雑な例

内部で管理する値が複数の場合

```haskell
-- コンストラクタ
robot (name, attack, hp) = \message -> message (name, attack, hp)
```

```haskell
-- インスタンス化
killerRobot = robot ("Killer3r", 25, 200)
```

```haskell 
-- ゲッター
getName aRobot = aRobot (\(n, _, _) -> n)
getAttack aRobot = aRobot (\(_, a, _) -> a)
getHP aRobot = aRobot (\(_, _, h) -> h)
```

```haskell
-- まとめて表示
printRobot aRobot = aRobot (\(n, a, h) -> n ++ "/" ++ (show a) ++ "/" ++ (show h))
```

```haskell
-- セッター
setName aRobot name = aRobot (\(n, a, h) -> robot (name, a, h))
setAttack aRobot attack = aRobot (\(n, a, h) -> robot (n, attack, h))
setHP aRobot hp = aRobot (\(n, a, h) -> robot (n, a, hp))
```

```haskell
-- 複数のオブジェクトを連携させる
damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10 then getAttack aRobot else 0
```

## 11.型の基礎

Haskellは他の関数型言語と比べると独特の型システムを持っている

### 基本の型

基本的には**型推論**により型が決定されるが、明示的に型を指定することも可能

```haskell
x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True
```

```haskell
values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: [Char]	-- Stringと同じ
letters = ['a', 'b', 'c'] -- "abc"と同じ
```

### 関数の型

関数にも型シグネチャがある

```haskell
double :: Int -> Int
double n = n * 2
```

```haskell
half :: Int -> Double
half n = (fromIntegral n) / 2 -- nをIntから汎用的な値へ変換する
```

### 文字列からの変換と文字列への変換

文字列「への」変換は**show**を使う
文字列「からの」変換は**read**を使う

```haskell 
show 6 -- "6"
show 'c' -- "'c'"
show 6.0 -- "6.0"
```

```haskell
-- 型推論に任せる
z = read "6"
q = z / 2

-- 明示的に型を宣言する ①変数に型を指定する
anotherNumber :: Int
anotherNumber = read "6"

-- 明示的に型を宣言する ②関数呼び出しの時に戻り値の型を末尾に指定する
read "6" :: Int
```

### 複数の引数を持つ関数のシグネチャ

複数の引数を持つ関数のシグネチャはこのように書く
**最後の型は常に戻り値の型**となる

```haskell
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)
```

実はHaskellの内部では**全ての関数が引数を1つしか受け取らない**ようになっており、**引数を複数受け取る関数は入れ子のラムダ関数**で実現されている

```haskell
makeAddress number street town = (number, street, town)

--本当はこうなってる
makeAddress = (\number -> (\street -> (\town -> (number, street, town))))
```

### 型変数

例えば複数の型を受けて問題なく動作するような関数はどうすればよいか
こういう時のために型変数という仕組みがある

```haskell
simple :: a -> a
simple x = x
```

## 12.カスタム型の作成

型に別名をつけたり、型を新しく作成したりすることができる

### 型シノニム(別名)

```haskell
type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int
```

上記のように定義しておくと下記の関数シグネチャは同じ内容になる

```haskell
patient :: String -> String -> Int -> Int -> String
```

```haskell
patient :: FirstName -> LastName -> Age -> Height -> String
```

### 新しい型を作成する

新しい型を作成する

data **型コンストラクタ** = **データコンストラクタ** | **データコンストラクタ**...

```haskell
data PhType = Pos | Neg
data ABOType = A | B | O | AB
```

複数の型を組み合わせて新しい型を定義することも可能

```haskell
data BloodType = BloodType ABOType RhType
```

インスタンス化はこんな感じで行う

```haskell
patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos
```

インスタンスに対して適用ができるメッセージ群(主にゲッター)

```haskell
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType :: (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo (BloodType AB _) _ = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False
```

### レコード構文

中括弧{}で囲んで型を定義する

```haskell
data Patient = Patient {
	name :: Name,
	sex :: Sex,
	age :: Int,
	height :: Int,
	weight :: Int,
	bloodType :: BloodType
}
```

インスタンス化はこんな感じ

```haskell
johnDoe :: Patient
johnDoe = Patient {
	name = Name "john" "Doe",
	age = 43,
	sex = Female,
	height = 62,
	weight = 115,
	bloodType = BloodType O Neg
}
```

レコード構文で作成したインスタンスにはゲッターやセッターが自動で作成される

```haskell
height johnDoe -- 62

johnDoeUpdated = johnDoe { age = 44 }
```