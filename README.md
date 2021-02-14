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

## ラムダ関数とレキシカルスコープ

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

## ファーストクラス関数(第一級関数)

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

## クロージャと部分適用

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

