# 問題: erl上で `<<"あ">>` とタイプしても `<<"\343\201\202">>` になってしまう

docker 上(CentOS)にて (`docker-compose exec api bash`) してから )

`+pc` オプションつきで erl を起動しても、`/usr/local/ymir/erlang/bin/erl +pc unicode` 日本語が表示されない。

具体的には、
コンソール上に `<<"あ">>` のようにタイプしたりコピペしたりしても、すぐ
`<<"\343\201\202">>` に変換されて表示される。

### 解決方法:

bash側の問題。

環境変数 LANG に en_US.utf8 を設定する

```
# export LANG=en_US.utf8
```

erlシェルを `+pc unicode` オプションをつけて起動すれば

```
# /usr/local/ymir/erlang/bin/erl +pc unicode
```

期待どおりに動く.

#### 詳細:

環境変数 LANG の設定を確認する

```
# locale
LANG=
LC_CTYPE="POSIX"
LC_NUMERIC="POSIX"
LC_TIME="POSIX"
LC_COLLATE="POSIX"
LC_MONETARY="POSIX"
LC_MESSAGES="POSIX"
LC_PAPER="POSIX"
LC_NAME="POSIX"
LC_ADDRESS="POSIX"
LC_TELEPHONE="POSIX"
LC_MEASUREMENT="POSIX"
LC_IDENTIFICATION="POSIX"
LC_ALL=
```

LANGが設定されていない、CTYPEなどもPOSIXになっているので、
設定する必要があることがわかる

使用可能なロケールを調べる

```
# locale -a
C
POSIX
en_AG
en_AG.utf8
en_AU
en_AU.iso88591
en_AU.utf8
en_BW
        ・
        ・
        ・
en_ZA
en_ZA.iso88591
en_ZA.utf8
en_ZM
en_ZM.utf8
en_ZW
en_ZW.iso88591
en_ZW.utf8
```

残念ながら `ja_JP` 系は存在しないので、
`en_US.utf8` を設定する。

```
# export LANG=en_US.utf8
```

## 補足:

### +pc オプションが効いているかの確認

[io:printable_range](http://erlang.org/doc/man/io.html#printable_range-0)

erl の起動オプション +pc は io:printable_range を設定するオプションです。
これは シェル、デバッガ、 io:format関数 が、どの範囲の文字（コード）であれば
そのまま渡しても画面に表示可能かを判断するための値です

laten1 | unicode のいずれかになります.

参考:

[2.7 インタラクティブシェル](https://gist.github.com/shkumagai/5270218#27-%E3%82%A4%E3%83%B3%E3%82%BF%E3%83%A9%E3%82%AF%E3%83%86%E3%82%A3%E3%83%96%E3%82%B7%E3%82%A7%E3%83%AB)


### Docker環境内でロケールエラーの設定

[CentOS7のlocaleを日本にする](https://qiita.com/silverskyvicto/items/5791740840296e6b6603)

この方法は、localectl な無いので、docker CentOSでは使えない.

[Dockerfileで日本語ロケールを設定する方法。およびロケールエラーの回避方法。| Qiita](https://qiita.com/YuukiMiyoshi/items/f389ea366060537b5cd9)

なので、上記方法(`locale -a` で使用可能なロケールを調べて、環境変数を直接設定する)

その他:

* [CentOS 7 コンテナに消えない日本語ロケールを追加する | Qiita](https://qiita.com/teruo-oshida/items/08cb84efc2b581b0a439)
* [docker exec /bin/bashでコンテナに入るとvimが文字化けする | Qiita](https://qiita.com/suin/items/23b21ce4e9a95f04965d)
* [Dockerコンテナで文字化けが発生した場合の直し方](https://yatta47.hateblo.jp/entry/2016/10/15/114701)
