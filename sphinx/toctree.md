

ビルドするとエラーがでる

```
/home/woo/reps/reports/src/report_2.rst: WARNING: document isn't included in any toctree
```

contents.rst に

```
:doc:`report_1`

:doc:`report_2`
```
のように書いてあったのでなぜ、と思った。


実はこう書くのが正解

```
.. toctree::
    :maxdepth: 2

    report_1
    report_2
```

## 参考

* [TOCツリー](http://docs.sphinx-users.jp/markup/toctree.html)
* [自分用技術メモの作成が捗ったSphinxのあれこれについて | Qiita](https://qiita.com/satake_masaki/items/7f73873809feab363b9f)
