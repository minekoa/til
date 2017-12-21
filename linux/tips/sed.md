# sed tips

* [sedコマンドの備忘録](https://qiita.com/takech9203/items/b96eff5773ce9d9cc9b3)
* [man](https://linuxjm.osdn.jp/html/GNU_sed/man1/sed.1.html)
* [sedでこういう時はどう書く?](https://qiita.com/hirohiro77/items/7fe2f68781c41777e507)
* [sedコマンド入門](https://sed.open-code.club/)

## 検索して置換

```
sed -e "s/hoge/piyo/"
```

`-e` は省略可能。

```
sed -e "s/hoge/piyo/"
```

デリミタは 実は `/` でなくともよい。 `s` の直後に来る文字がデリミタになる

sed -e "s@hoge@huga@"


## 複数の条件

`-e` オプションをつかう。左から順に実行される。

```
cat foo.txt | sed -e "/abc/ABC/" -e "/xyz/XYZ"
```

## -s オプション とは？

`-s`, `--separate`
複数の入力ファイルを一続きのストリームとして扱わずに個別のファイルとして扱う


## 行の削除

`sed "s/^#.*//" ` をしても改行文字は残ってしまう。行を削除したい場合こうする。

```
sed "/^#.*/d"
```

`s` はいらない

## 範囲削除

```
sed -e "/aaa/,/bbb/d" 
```

`aaa` を含む行から `bbb` を含む行までを削除

