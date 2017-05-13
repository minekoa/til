# 第2章 式、型、値

## 2.8 練習問題

## 練習問題A

> 2 たす 2 の半分は 2 かそれとも3か

```
(2+2)/3 | 2+(2/3) == 2 | 3
```

なので、「そのとおり」

### 練習問題B

#### `[0,1)`

```haskell
Prelude> [0,1)

<interactive>:1:5: error: parse error on input ‘)’
```

#### `double -3` `double (-3)` `double double 0`

```haskell
Prelude> let double = (* 2) :: Int -> Int
```

とする。

```haskell
Prelude> double -3

<interactive>:4:1: error:
    • No instance for (Num (Int -> Int)) arising from a use of ‘-’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: double - 3
      In an equation for ‘it’: it = double - 3
Prelude> double -(3::Int)

<interactive>:5:10: error:
    • Couldn't match expected type ‘Int -> Int’ with actual type ‘Int’
    • In the second argument of ‘(-)’, namely ‘(3 :: Int)’
      In the expression: double - (3 :: Int)
      In an equation for ‘it’: it = double - (3 :: Int)
```

負数のリテラルもなければ、単項前置の`-`もない。Haskell の直感的でないところ。

なので () で囲う。


```haskell
Prelude> double (-3)
-6
Prelude> :t (-3)
(-3) :: Num a => a
Prelude> :t (+ (-3))
(+ (-3)) :: Num a => a -> a
Prelude> let x = -3 in (+ x)

<interactive>:9:1: error:
    • No instance for (Show (a0 -> a0)) arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

```haskell
Prelude> double double 0

<interactive>:10:1: error:
    • Couldn't match expected type ‘Integer -> t’
                  with actual type ‘Int’
    • The function ‘double’ is applied to two arguments,
      but its type ‘Int -> Int’ has only one
      In the expression: double double 0
      In an equation for ‘it’: it = double double 0
    • Relevant bindings include it :: t (bound at <interactive>:10:1)

<interactive>:10:8: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Int -> Int’
    • Probable cause: ‘double’ is applied to too few arguments
      In the first argument of ‘double’, namely ‘double’
      In the expression: double double 0
      In an equation for ‘it’: it = double double 0
```

かなり親切に説明してくれているが、「ひょっとして、うしろのdoubleを()で囲い忘れているのじゃないですか？」とは教えてくれない。
ここらへん、機械学習でエラーの原因をいい感じに教えられる治験をためればいいんじゃないかな、という話題が
Haskell界隈であったとかなかったとか。


#### `if a == 0 then 2 == 1`

```haskell
Prelude> if 1 == 0 then 2 == 1
<interactive>:12:22: error:
    parse error (possibly incorrect indentation or mismatched brackets)
```

else が足りない。我々は式なので！

(ということは、Maybe を返すようなのがあればいいかな？)


#### "++" == "+" ++ "+"

```haskell
Prelude> "++" == "+" ++ "+"
True
```

#### `[(+), (-)]`

```haskell
Prelude> [(+), (-)]

<interactive>:13:1: error:
    • No instance for (Show (a0 -> a0 -> a0))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

式としては正しいが、演算子をshowする方法がないので、エラーになっている。


```haskell
Prelude> :t [(+), (-)]
[(+), (-)] :: Num a => [a -> a -> a]
```



#### `[[],[[]],[[[]]]]`

かっこ、こっか、がっきん、ごっきん

```haskell
Prelude> :t [[],[[]],[[[]]]]
[[],[[]],[[[]]]] :: [[[[t]]]]
```

ちなみに表示されるのは、最も一般化されたもの←弱い型.
 t が `[t]` でも `[[t]]` でも `[[[Int]]]` でもいい。


たとえば

```haskell
Prelude> :t [[],[[]],[[[]]]] :: [[[[[[a]]]]]]
[[],[[]],[[[]]]] :: [[[[[[a]]]]]] :: [[[[[[a]]]]]]
```

となる。

#### `concat ["tea", "for", '2']` `concat ["tea", "for", "2"]`

前者はリストの中の型が一致しない。われわれは動的言語ではないので、

### 練習問題C

> かつては論文のタイトルを以下のように書くことができた
> > The morphology of prex -- an essay in meta-algorithmics
>
> 最近では、すべての単語の先頭を大文字で書くよう求められることが多いようである.
>
> > The Morphology Of Prex -- An Essay In Meta-Algorithmics"
>
> 上のような論文タイトルの大文字化を行う関数 `String -> String` をかけ.

> 関数 `words :: String -> [Word] は前章で使った関数である．プレリュード関数 `unwords :: [Word] -> String` は何をする関数だと思うか. ヒント: 以下の等式の一方だけが成り立つとすればどちらか
> ```
> words . unwords = id
> unwords . words = id
> ```

p18 で word は空白文字は含まないことを想定している（ただし、型では示せない）
だから、気分としては、`["a", "", "b"]` は型エラー。

```haskell
Prelude> import Data.Char
Prelude Data.Char>  { capitalize :: String -> String; capitalize = unwords . map cap . words where cap xxs = [toUpper (head xxs)] ++ tail xxs }
Prelude Data.Char> capitalize "The morphology of prex -- an essay in meta-algorithmics"
"The Morphology Of Prex -- An Essay In Meta-algorithmics"
```

素朴に解くと、あれ、`Meta-Algorithmics` にならない。
意外に厄介。


### 練習問題D

先行評価と遅延評価の話。

> `head . filter p` を、pを満たすリストの最初の要素を探す関数として使える。
> Beaver ではこの関数を使わない理由をのべよ.
>
> その代わり Beaver では以下のような定義した関数を使うようになる。
> 
> (中略)
> 
> Beaver用の `first` 関数の右辺を完成させよ

```haskell
first :: (a -> Bool) -> [a] -> a
first p xs  | null xs   = error "Empty list"
            | p x       = x
            | otherwise = first (tail xs)
            where
               x = head xs
```

遅延評価のよいところは、部品関数を素朴に組み合わせることが出来る。
正確評価では、効率や停止性を考え、再帰で書くことになる。

真のモジューラビリティを得られるところ。

2.10. 章末ノートより

> つまり、正確評価のもとでは、効率の点からプログラムの定義は, 多くの場合, 明示的再帰を用いた
> ものにならざるを得ないということである。そうなると、単純な標準関数から定義を組み立てることが
> できなくなり、部品となる関数が持つ法則を適用するという方法では
> 関数に関する論証ができないことになる.

### 練習問題E

> `Maybe` 型は標準プレリュードでは以下のように宣言されている
>
> ```
> data Maybe = Nothing | Just
>             deriving (Eq, Ord)
> ```

`deriving` ってなんだ？、とおもったらすぐ書いてあった

> この宣言は `deriving` 説を使っている。Haskellではいくつかの標準型クラスのインスタンスを自動で生成できる型がある.


```haskell
first :: (a -> Bool) -> [a] -> Maybe a
first p xs  | null xs   = Nothing
            | p x       = x
            | otherwise = first (tail xs)
            where
               x = head xs
```


> 最後に Maybe a -> Maybe a 型の関数はいくつあるか数えよ

非常に盛り上がりつつ難題。

* Strict

```
Nothing -> ⊥, Nothing, Just ⊥
Just a  -> ⊥, Nothing, Just a, Just ⊥
```

*  Non Strict

```
id                   --NG
const Nothing
const (Just ⊥)
```

id の定義は `id ⊥ === ⊥` なので、id は Non Strict にしかなりえない。
（関数の性質としても、評価の結果としても）

非正確関数は、⊥ をわたして⊥を返したらダメ

```
f ⊥ = ⊥      --Strict
f' ⊥ = ■     --Nos Strict
```

なので、idはNG。

あとひとつは、回答をみると

```
\x -> Just( case x of
  Nothing -> ⊥
  Just v -> v)
```

これが Non Strict なのはなぜかという話で、case が遅延評価するはなし、
しないという話で紛糾した。

```haskell
Prelude> let z = Just(case [] of {_:_ -> 1; [] -> 0})
Prelude> :print z
```

GHC 8 系だとちゃんと出ない。


```haskell
Prelude> import Data.Maybe
Prelude Data.Maybe> isJust ( (\x -> Just( case x of {Nothing -> undefined; Just v -> v})) undefined)
True
```

case が遅延しないとすると、
case x で undefined を触ってしまうと、例外に起きて全体が undefine に落ちる（例外がおきる）はず

なのに、isJust が True になるということは、case が遅延している(サンクがある）ということ？

case は遅延しないけれど、変数は遅延するということだった。
「遅延しないね、おかしいね」というサンプルは、`case [] of` で、データコンストラクターだった。それで遅延しない。

### 練習問題F

> 半可通プログラマの尻高くんは

「尻高」→「しったか」

### 練習問題G


### 練習問題H

```haskell
type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum cin =
    cin ++ show (n `div` 10) ++ show (n `mod` 10)
  where
    n = sum (map getDigit cin)

valid :: CIN -> Bool
valid cin =
  cin == addSum (take 8 cin)
```

`take` は 文字数が 8文字以上のとき、チェックサムが合わず、8文字以下のとき
文字数が合わないので、lengthチェックが不要。


### 練習問題I

> 対話プログラム
>
> ```
> palindrome :: IO ()
> ```
>
> を書け. このプログラムを走らせると以下のような対話セッションが出来るものとする.
>
> ```
> ghci> palindrome 
> Enter a string:
> Madam, I"m Adam
> Yes!
>
> ghci> palindrome 
> Enter a string:
> A Man, a plan a canal - Suez!
> No!
> ```

`A Man, a plan a canal - Suez!` は、`canal - Panama` になれば回文になるというネタ。


```haskell
import Data.Char (toLower, isAlpha)

palindrome :: IO ()
palindrome =
  do { putStrLn "Enter a string"
     ;  s <- getLine
     ;  putStrLn (if isPalindrome s then "Yes!" else "No!")
     }

isPalindrome :: String -> Bool
isPalindrome s =
    reverse s' == s'
  where
    s' = map toLower (filter isAlpha s)
```

interact をつかい、すべてを関数合成でかくバージョン

```haskell
import Data.Bool (bool)

palindrome' :: IO()
palindrome' = interact f
  where
    f =
      unlines
      . ("Enter a string:" :)
      . map (bool "No!" "Yes!" . isPalindrome)
      . lines
```

Sコンビネータでかいたりもしてたが、そちらはみてない。

