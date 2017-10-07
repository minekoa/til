# 第4章 リスト

## 4.1 リスト記法

```
Prelude> filter (< 4) [1..]
[1,2,3
```

止まらない...

これは、`1:2:3:undefined`

これは止まる

```
Prelude> take 2 $ filter (< 4) [1..]
[1,2]
```

結局、undefined に触ってしまうと止まらなる。

## 4.2 列挙

x < y という制約は正しい、という意味。

x = y になることはない。なぜならば、2x<sup>2</sup> は整数の二乗にならないから。
だから x < y という条件でいい。


## 4.3 リスト内包表記

## 4.4 基本演算

```
case e of
   Nothing ->
   Just r   :t
```

> (ここで問題. なぜ, 単純に `null = (== [])` と定義しないのか.)

空リストは型が定まらない
というより、データ構造の性質だけで十分なのに、要素の型の性質が入り込んでしまう

(幽霊型を持っている直和型なら)

だんだんこういうのがキモくなってくる

(余談) `== True` もキモくねぇ？ `== False` は `!` が not な言語なら、それが見にくいののであり。


(第四回: 2017-10-01)

## 4.5. 連接

```
Prelude> [1,2] ++ undefined
[1,2*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:1:10 in interactive:Ghci1
```

```
Prelude> head $ [1,2] ++ undefined 
1
Prelude> take 2 $ [1,2] ++ undefined 
[1,2]

```

note: パターン照合すると strict になる

## 4.6 concat, map filter

map のファンクタ則

parametric 多相になっている関数は、自然変換(natural transformation)になっている。
（自然性(naturarlity)がある、ともいう）

(関数を適用する前に値を変換するか、あとに変換するかは交換可能) .. パラメトリック多相

```
map f . tail = tail . map f

```

可換図式

```
length :: [a] -> Int

   
           map f
        [a] ----> [b]
 length  |         |
         V         V
        Int ----> Int
             id

length . map f = id . length
```
(length を適用したら Intに潰れる)

map f という関手を高等関手をに変換する


```
tail :: [a] -> [a]

f :: a -> b

       map f
    [a] ---> [b]
tail |        |
     V        V
    [a] ---> [b]
        map f
```


`filter p. map f concat = concat . concat`

可換図式に起こしてみる

```
filter  :: (a -> Bool) -> [a] -> [a]

f  :: a -> b
p' :: a -> Bool
p  :: b -> Bool

p' = p. f

              map f
          [a]  -->  [b]
filter p'  |         |    filter p
           V         V
          [a]  -->  [b]
              map f
```

`filter p` とかを除去してみる

```
p' = p . f

inv f :: b -> a
p' . inv f == p . f . inv f == p


                  ((. inv f) ***  map f)
              (p, [a])  ------>  (p', [b])
uncurry filter   |                  |    uncurry filter
                 V                  V
                [a]     ------>    [b]
                        map f

```



f . inv f が id にならないケース

```
f : a ----> 1
    b

inv f : 1 ---> a

f . inv f === id

invf . f /= id
```

`inv` の定義ではない。 `f . inv f === id` が `inv`

### 4.7 zip と zipWith

```
zip = zipWith (,)
```

`(,) a b = (a,b)`なので。

---

リストが非現象列になっていることを判定する関数

```
nondec xs = and (zipWith (<=) xs (tail xs))
```

このパターンはよく使う。

パスワードのポリシーチェック、３文字同じ文字が続かないことの確認
tail tail ２つ繋げた

---

```
position :: (Eq a) => a -> [a] -> Int
position x xs = head ([j | (j, y) <- zip [0..] xs, y == x] ++ [-1])
```

* `++ [-1]` は番兵だと思えばいい
* 普通はMaybeだよね
* 最近のGHCなら NonEmpty型がある

```
Prelude> import Data.List.NonEmpty
Prelude Data.List.NonEmpty> :i NonEmpty 
data NonEmpty a = a :| [a] 	-- Defined in ‘Data.List.NonEmpty’
instance Eq a => Eq (NonEmpty a) -- Defined in ‘Data.List.NonEmpty’
instance Monad NonEmpty -- Defined in ‘Data.List.NonEmpty’
instance Functor NonEmpty -- Defined in ‘Data.List.NonEmpty’
instance Ord a => Ord (NonEmpty a)
  -- Defined in ‘Data.List.NonEmpty’
instance Read a => Read (NonEmpty a)
  -- Defined in ‘Data.List.NonEmpty’
instance Show a => Show (NonEmpty a)
  -- Defined in ‘Data.List.NonEmpty’
instance Applicative NonEmpty -- Defined in ‘Data.List.NonEmpty’
instance Foldable NonEmpty -- Defined in ‘Data.List.NonEmpty’
instance Traversable NonEmpty -- Defined in ‘Data.List.NonEmpty’
```

tail すると型が変わってしまう

```
Prelude Data.List.NonEmpty> :t uncons
uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
```
Maybe ついてる

---

## 4.8 頻出単語（完成編）

## 4.9

### 練習問題A

```
[] : xs = xs
[] : xs = [[], xs]

```

ひとつ目は

```
Prelude> :t [] : [1,2,3]
[] : [1,2,3] :: Num [t] => [[t]]
Prelude> :t [1,2,3]
[1,2,3] :: Num t => [t]
```

なので、だめなんじゃないｚ・
→ xs が ListのListならいいんじゃない？

```
Prelude> :t [] : [[1]]
[] : [[1]] :: Num t => [[t]]
Prelude> :t [[1]]
[[1]] :: Num t => [[t]]
```

---

へんなデフォルティングが走る

:set -XNoMononorphismRestriction


null' [1 ::: ???] :: (Eq a, Num a) => Bool


### 練習問題B

```
allPairs' = 
    [(x, y) | x <- [0 ..], y <- [0 .. x]]
```

かなぁ。

解:

```
allParis' :: [(Integer, Integer)]
allPairs' =
    [ (x, y)
    | z <- [0 ..]
    , x <- [0 .. z]
    , let y = z - x
    ]
    
main:: IO ()
main print $ take 20 allPair'
```

> 自然数の対で互いに相異なるものすべての無限リスト

たとえば(1,1)はだめ？という意味？


### 練習問題C

```
disjoint :: (Ord a) => [a] -> [a] -> Bool
```

### 練習問題D

式変形してみる

```
[e | x <- xs, p x, y <- ys]

concatMap (\x -> [e | p x, y <- ys]) ys

concatMap (\x -> if (p x) then [e | y <- ys] else []) xs

concatMap (\x -> if (p x) then concatMap (\y -> [e]) ys else []) xs


[e | x <- xs, y <- ys p x]

concatMap (\x -> [e | y <- ys, p x]) xs

concatMap (\x -> concatMap (\y -> [e| p x\) ys) xs

concatMap (\x -> concatMap (\y -> if (p x) then [e] else []) ys ) xs
```

ys が x に依存すれば差がでるのでは？

```
mkYS :: Integer -> [Integer]
mkYS x
  | odd x     = [1..]
  | otherwise = [0]
```

```
*Main> [(x,y) | x <- [0..3], even x, y <- mkYS x]
[(0,0),(2,0)]
*Main> [(x,y) | x <- [0..3], y <- mkYS x, even x]
[(0,0)
```

後者は非停止。

--- 答えを見てみよう

```
*Main> [1 | x <-[1,3], y <- [1..], even x]
^CInterrupted.
*Main> [1 | x <-[1,3], even x, y <- [1..]]
[]
```


### 練習問題E

```
-- a^3 + b^3 = c^3 + d^3
-- a < c <= d < b

ramanujanNumbers :: [(Integer, Integer, Integer, Integer)]
ramanujanNumbers =
  [ (a, b, c, d)
  | b <- [1   .. ]
  , d <- [1   .. b -1]
  , c <- [1   .. d]
  , a <- [1   .. c -1]
  , a^3 + b^3 == c^3 + d^3
  ]
```

```
*Main> take 10 ramanujanNumbers 
[(1,12,9,10),(2,16,9,15),(2,24,18,20),(10,27,19,24),(4,32,18,30),(2,34,15,33),(9,34,16,33),(3,36,27,30),(17,39,26,36),(12,40,31,33)]
*Main> map (\ (a,b,c,d) -> a^3 + b^3) $ take 10 ramanujanNumbers 
[1729,4104,13832,20683,32832,39312,40033,46683,64232,65728]
```

```
*Main> take 20 ramanujanNumbers 
[(1,12,9,10),(2,16,9,15),(2,24,18,20),(10,27,19,24),(4,32,18,30),(2,34,15,33),(9,34,16,33),(3,36,27,30),(17,39,26,36),(12,40,31,33),(4,48,36,40),(6,48,27,45),(12,51,38,43),(8,53,29,50),(20,54,38,48),(17,55,24,54),(9,58,22,57),(5,60,45,50),(3,60,22,59),(8,64,36,60)]
*Main> map (\ (a,b,c,d) -> a^3 + b^3) $ take 20 ramanujanNumbers 
[1729,4104,13832,20683,32832,39312,40033,46683,64232,65728,110656,110808,134379,149389,165464,171288,195841,216125,216027,262656]
```

このやりかただと、d 順になる Ramanujan数 順にするには、、といろいろ試行錯誤したら遅い。


### 練習問題H





