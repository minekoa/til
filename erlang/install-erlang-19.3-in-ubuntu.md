# Erlang 19.3 をソースコードからインストールする

Erlang 19.3 をChromebook上のubuntuにインストールします。(Chromebook Flip C302CA, crouton xenial)

まずはビルドに必要な一通りをインストール。

```
$ sudo apt install build-essential libncurses5-dev libssl-dev curl
```

ソースコードを取得して展開。

```
$ curl -LO http://erlang.org/download/otp_src_19.3.tar.gz
$ tar -zxvf otp_src_19.3.tar.gz
$ cd otp_src_19.3
```


```
$ ./configure --prefix=$HOME/usr/local --enable-hipe --enable-dtrace --without-javac 2>&1 | tee ../erl-configure.log
```

結果、

```
*********************************************************************
**********************  APPLICATIONS DISABLED  **********************
*********************************************************************

jinterface     : Java compiler disabled by user
odbc           : ODBC library - link check failed

*********************************************************************
*********************************************************************
**********************  APPLICATIONS INFORMATION  *******************
*********************************************************************

wx             : wxWidgets not found, wx will NOT be usable

*********************************************************************
*********************************************************************
**********************  DOCUMENTATION INFORMATION  ******************
*********************************************************************

documentation  : 
                 xsltproc is missing.
                 fop is missing.
                 xmllint is missing.
                 The documentation can not be built.

*********************************************************************
```

例によって、`jinterface` `odbc` `wx` `documentation` が使えないよ、というメッセージが出力されますが、
そして前回はwxErlang が欲しくって自前ビルド指定のですが、今回はwxErlang は使わないつもりなので、これはありということで。

```
$ make 2>&1 | tee ../erl-make.log
$ make install 
```


## 参考

* [wxErlang が動きませんよ? ](http://d.hatena.ne.jp/minekoa/20160104/1451890224) .. 前回Chromebookにインストールした時のメモ。
* [Erlang/OTP 19.3 をソースコードからインストールする](http://qiita.com/voluntas/items/a39cf3ec1a385e612f2a) .. ジンジャーさんの記事
