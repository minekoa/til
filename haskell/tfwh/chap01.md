# 1. 関数プログラミングとはなにか

Haskell による関数プログラミングの思考法 読書会
2017-03-03 13:00- 歌舞伎座タワー21F

[TFwH 第1章 関数プログラミングとは何か](http://tfwh.sampou.org/docs/TFwH-Chap01.html)

## 1.1 関数と型

> Intは-2^29 ≦ n ≦ 2^29 な範囲にあるnのような*1

> *1 訳注: 最新の仕様 Haskell 2010 では Int の範囲は **少なくとも** この範囲をカバーすることが要請されている. 実際には minBound :: Int および maxBound :: Intの値がそれぞれIntの下限および上限を表している

やる。

```
Prelude> maxBound :: Int
9223372036854775807
Prelude> minBound :: Int
-9223372036854775808
Prelude> 2^63 -1 -- (maxBound :: Int)
9223372036854775807
Prelude> 2^63 -1 == (maxBound :: Int)
True
Prelude> -2^63 == (minBound :: Int)
True
Prelude> 
```


> ところで、HaskellではlogはlogBase e の短縮形である


でも haskell では e は定義されていない。exp ならある？

```
Prelude> :t exp
exp :: Floating a => a -> a
Prelude> exp 1
2.718281828459045
```

## 1.2 関数合成 / 1.3 例題: 頻出単語

関数合成とその例題。

http://tfwh.sampou.org/docs/TFwH-Chap01.html#g:4
> ### commonWords :: Int -> Text -> String Source #
>  
> commonWords n はテキストから頻出単語の出現回数の上位 n 個の単語を出現回数とともに表示するための文字列を生成する．
>  
> ```
> commonWords n = concat       -- 連結する
>               . map showRun  -- 単語と出現回数をを表示する文字列を生成する
>               . take n       -- 上位 n 個を取り出す
>               . sortRuns     -- 出現回数順でソートする
>               . countRuns    -- 出現回数を数えて単語と出現回数を対にする
>               . sortWords    -- 単語をソートする
>               . words        -- テキストを単語に分解
>               . map toLower  -- テキストの文字をすべて小文字にする
> ```

と、関数合成でアルゴリズムを組み立てていく。これを一番最初の例として持ってくることが
Bird 先生が前書きでいう「しかし, 関数プログラミングが近年まれに見る再考の技法である理由, すなわち, 関数ぷろう柄編むを数学的に考察出来るという点を強調するものはほとんどない.」なこだわりの表明なのかと感じる。


> *3 訳注: 関数適用は関数合成演算子よりも優先順位が高い（関数適用はあらゆる中置演算子より優先順位が高い）. しあがって `words . map toLower` は `words . (map toLower)` のことである.

(余談)関数適用演算子 `$` というものがある。

```
sin $ theta
```

```
Prelude> :i $
($) ::
  forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
  (a -> b) -> a -> b
  	-- Defined in ‘GHC.Base’
infixr 0 $
```

ただし `$` は結合力が一番が弱い。


## 1.4 例題: 数を言葉にする

http://tfwh.sampou.org/docs/src/TFwH.NumbersIntoWords.html#convert

文芸的プログラミングをHaskellで行う、文芸的Haskellスクリプト(Literate Haskell Script)。
拡張子は `.lhs` 。行頭が `>` であれば、プログラムの行となる。

注意: この本では、拡張子は .lhs で行きつつも、本文中のコードの表記では以降 `>` を省略するとしていて、
非常に紛らわしい。

あと、WinGHC を前提にかかれている。


## 練習問題A

### やってみる

```
*Main> map double [1, 4, 4, 3]
[2,8,8,6]
*Main> map (double . double) [1, 4, 4, 3]
[4,16,16,12]
*Main> map double []
[]
```

> 以下の表明が真であるか. それはなぜか.
> 
> ```
> sum . map double = double . sum
> sum . map sum    = sum . concat
> sum . sort       = sum

ちなみに sum :: [Integer] -> Integer -- 整数のリストの和を求める




> concat が何をする関数か思い出すこと

```
*Main> :i concat
concat :: Foldable t => t [a] -> [a] 	-- Defined in ‘Data.Foldable’
```

-> リストのリストをリストにする

```
*Main> (sum . map sum) [[1,2],[3,4]]
10
*Main> (sum . concat) [[1,2],[3,4]]
10
```

### 解答編

→ これはちゃんと証明せよ

[TFwH.Chap01.ExA](http://tfwh.sampou.org/docs/TFwH-Chap01-ExA.html)

実は sort の証明がめんどい。

## 練習問題B

sin<sup>2</sup>θ

これは、 (sin theta) ^ 2 という意味なので、

```
(sin theta) ^ 2
```

また、関数適用は中置演算子よりも優先度がたかいのでカッコは外してもOK

```
sin theta ^ 2
```

### 練習問題C

```
Prelude> :t 'a'
'a' :: Char
Prelude> :t "a"
"a" :: [Char]
```

## 練習問題D

1. 全てのテキストの文字を小文字に変換してから、単語に分ける
2. まず単語に分けてから、各単語の文字を小文字に変換

1は以下である

```
words . map toLower
```

では2は？、という問題。

### やってみる

f (g x) = (f . g) x

なので、words ( map toLower x) になる。

ならば、map toLower (words x) は

```
map toLower . words
```

になるはず。


## 練習問題E

結合性とは以下のように定義される

```
x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z

```

1. 数値上の加法は結合性を持つか
2. リスト上の連接は結合性を持つか
3. 関数合成は結合性をもつか
4. 数値上の演算で結合性を持たないものの例をあげよ

また、1, 2, 3 の単位源はなに？

### 単位元は?

1 加法 → 1
2 リスト上の連接 → 空リスト
3 id関数

### 1.数値上の加法は結合性を持つか

もつ

```
(1 + 2) + 3 = 1 + (2 + 3)
(3) + 3 = 1 + (5)
6 = 6
``
#### 解答編

→数学上の話だとありだけど、計算機上だとどうじゃろ

float , double はある

Haskell の Int は体になっているので大丈夫（オーバーフローを考えればだめそうだけど、ラウンドされる？とかいってた？）

やってみる

```
Prelude> let x = maxBound :: Int
Prelude> let y = 3
Prelude> let z = 5
Prelude> (x + y) + z == x + (y + z)
True
Prelude> x + y + z
-9223372036854775801
```

結果は変だが上記定義によれば、結合性はもつ

持たない例。

```
Prelude> let [x,y,z] = [1.0e-100, 2, -2] :: [Double]
Prelude> (x + y) + z == x + (y + z)
False
```

ちなみに

```
Prelude> x + y
2.0
Prelude> y + z
0.0
```

### 2. リスト上の連接は結合性を持つか

リスト上の連接ってなんだっけ？

→　++

なので、持つ。

### 3. 関数合成は結合性をもつか

もたないかな。
sort と concat で適用順かえてみるとかで例示できるとおもう

#### 解答編

→もつ。あうち。

遅延評価で順序変えても良いからそっかー。

### 4. 数値上の演算で結合性を持たないものの例をあげよ

実数の除算とか、四元数の乗算とか

```
(12 / 6) / 3 = 12 / (6 / 3)
    2    / 3 = 12 /    2
   0.666...  = 6
```

#### 解答編

減算、冪乗


## 練習問題 F

### 解答編

http://tfwh.sampou.org/docs/TFwH-Chap01-ExF.html

```
$ stack runghc app/anagrams.hs 6 < /usr/share/dict/words 
```
とかで実行する

### 練習問題 G

正誤表をみよ（原著から回答がまちがってたりする）

## 1.8 章末ノート

> その名が示すように2冊めは高度に実用的な応用について書かれており, 3冊めは入門のための教科書である. Graham Hutton は著者にニヤリとわらって, 本書は(Real World(実世界)Haskell) ではなく Ivory Tower (象牙の塔) Haskell と呼ぼうといった.

ちなみに Programming in Haskell は第二版がでているとのこと。邦訳が進んでいるのかはわからないとのこと。

