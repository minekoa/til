# 第5章 単純な数独ソルバー

## 5.1 仕様


```
digits :: [Char]
```

なんで `Digit` のリストじゃないの？（原著がそうなってる、たぶん。ミスっぽい）


```
$ stack ghci
Configuring GHCi with the following packages: 
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/ghci10564/ghci-script
Prelude> type Row a = [a]
Prelude> type Matrix a = [Row a]
Prelude> type Digit = Char
Prelude> type Grid = Matrix Digit
Prelude> digits = ['1' .. '9']
Prelude> blank = (== '0')
Prelude> choise d = if blank d then digits else [d]
Prelude> choices = map (map choise)
Prelude> :t choices
choices :: [[Char]] -> [[[Char]]]
Prelude> type Choices = [Digit]
Prelude> :t choices :: Grid -> Matrix Choices 
choices :: Grid -> Matrix Choices :: Grid -> Matrix Choices
```



### `[x : ys | x <- xs, ys <- cp xss]` が効率悪い理由

（cp : cartesian product : デカルト積）

```
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]
```

より

```
cp (xs : xss) =  [x : ys | x <- xs, yx <- yss]
    where
     yss = cp xss
```

が効率通いとされる。

内包表記はdoで書いているのと一緒。

```haskell

cp (xs : xss) = [ x : ys | x <- xs , ys <- cp xss ]

cp (xs : xss) = do
  x <- xs
  ys <- cp xss
  return $ x : ys

cp (xs : xss) =
  xs     >>= \x ->
  cp xss >>= \ys ->
  return( x : ys)

cp (xs : xss) =
  concatMap (\x ->
   cp xss >>= \ys ->
   [x : ys] )
  xs


cp (xs : xss) =
  concatMap
  (\x -> concatMap (\ys -> [x : ys]) (cp xss) xs

-- 外側のconcatMapがダメ


concatMap f yss = cncast (map f yss)
```

## 5.2 法則を使ったプログラムの構成

以下の三法則を証明

* `rows . rows = id`
* `cols . cols = id`
* `boxs . boxs = id`

(これを対合性(たいごうせい、ついごうせい のどちらのよみでもよい）という）

`rows . rows` は `Matrix` が row のListであることから自明

### boxs . boxs が元に戻ることの証明

(cols . cols は証明されているとする。この証明は難しいとのこと）

`boxs` の定義を用いて `boxs . boxs` を書き換える。で、どんどん潰す。

```
map ungroup . ungroup . map cols . group . map group . map ungroup . ungroup .  map cols . group . map group
                                           ~~~~~~~~~~~~~~~~~~~~~~~                                                map group . map ungroup -> id
                                   ~~~~~              id             ~~~~~~~                                      group . ungroup         -> id
                        ~~~~~~~~                                                ~~~~~~~~                          map cols . map cols     -> id
              ~~~~~~~~                                                                    ~~~~~~~                 ungroup . group         -> id
~~~~~~~~~~~~                                                                                       ~~~~~~~~~~     map ungroup . map group -> id
```

## 5.3 選択肢行列の枝刈り

リスト内包表現でのパターンマッチ

```
Prelude> foo = [[1, 2], [3], [4], [5, 6, 7]]
Prelude> [d | [d] <- foo]
[3,4]
```

要素が一個しかないもののみをパターンマッチしている

```
> pruneRow ["6", "12", "3", "134"]
["6","12","3","14" ]
```


> pruneBy f = f . map pruneRow . f である

f は対合性があるので、f で変換してpuruneして、元にもどすという処理のこと

> 精緻化する
精緻化 = refine
