# 問2-ii



## ビルド・実行方法

事前条件として、`ymlsq/pj1/log.csv` `ymlsq/pj1/log_1G.csv` の２つのファイルが必要。

`log.csv` は問1 のプログラムを実行していれば生成されるので、一度実行して `log_1G.csv` にリネームし、
再度実行して準備しておく。


準備が整ったならば、

```
$ make run
```

でビルド、実行される。（ビルドだけならば、`make` で OK)
ビルドには、C++1z (17) をビルド可能なバージョンの gcc が必要。 

実行結果は`out.txt` に出力され、
コンソールに　`/usr/bin/time -f "%M(KB) e=%E s=%S u=%U"` の結果が表示される。


## コマンドの使い方

生成された `a.out` は、引数に ログインIDを照合する CSVファイルのパスを２つ取る。

```
$ ./a.out path/to/log1.csv path/to/log2.csv
```

