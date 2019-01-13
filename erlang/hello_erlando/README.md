hello_erlando
=====

An escript

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/hello_erlando




## これはなに？

学習用のプロジェクト

* erlando になれる
* rebar3 の雛形 escript を実際に使ってみる



erlando とは、Erlang で do記法とscheme のcutsマクロを実現するためのライブラリ。
do記法は maybe なんかで case ネストで大変なことになるのを防いだりとかが
主な用途。

cutマクロは、Erlang は カリー化サポートしないから クロージャで
くるむ必要があるのめんどくない？という動機。



学習課題は以下の記事を参考にした。

[erlando - ErlangでMaybeモナドとdo記法を使う](https://qiita.com/tatsuya6502/items/9f43d1b4486f0416ac68)




