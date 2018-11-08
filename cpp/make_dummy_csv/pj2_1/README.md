# 問2-i

## 実行方法

実行前の条件として、`ymlsq/pj1/log.csv` ファイルが存在すること。
（問1 のプログラムを実行していれば生成されている）

```
$ cd src
$ ./run.bat
```

で実行される。実行には Python3系が必要。

集計結果は、`src/summary.csv` ファイルに出力され、
コンソールに　`/usr/bin/time -f "%M(KB) e=%E s=%S u=%U"` の結果が表示される。

## スクリプトの使い方

`src/aggregation_domain.py` がスクリプトの本体である。
引数にドメイン別集計を行う対象のCSVファイルへのパスを指定する

```
$ ./aggregation_domain.py path/to/log.csv
```

