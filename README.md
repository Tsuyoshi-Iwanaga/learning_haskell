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

