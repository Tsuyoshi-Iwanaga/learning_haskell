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

