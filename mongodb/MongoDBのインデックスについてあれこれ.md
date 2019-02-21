# MongoDB のインデックスについてアレコレ

とある社内ドライバーで付与可能だった属性について
よくわかんなかったので調べながらお勉強した

* インデックスの属性
    * [-] unique :: Boolean
    * [x] sparse :: Boolean
    * [x] expireAfterSeconds :: NonNegInteger
* インデックスに指定されたフィールドの属性
    * [x] field/direction :: ascending | descending

## sparse

sparse ..希薄？

引用: [MongoDBのsparse indexについての雑なまとめ](http://blog.kjirou.net/p/4257)

> 本来は「指定フィールドが存在しない場合にインデックスをしない」というもの


引用: [PHP: MongoCollection::ensureIndex - Manual](http://php.net/manual/ja/mongocollection.ensureindex.php)

> "sparse"
>
> TRUE を指定すると、疎なインデックスを作ります。これは、指定したフィー
> ルドを含むドキュメントだけをインデックスします。デフォルト値は FALSE
> です。



フム。で、最初のブログで問題定期されている

> ## unique と一緒に指定すると null を重複判定から除外できる？
>
> ググって昔の日本語記事を読むとこういう解説が良く出てきますが、不正確です。
> どちらかというと間違いです。

は、これを応用（悪用？）するパターンとして
インデックスが作られない→重複しても怒られない（というか無いものは重複しない）
となるわけだけど、「フィールドが無い」と「フィールドの値がnull」を取り違えると
重複になるわけだ

(本当は 一階述語論理の Null は unknownの意を含むので、equalマッチさせちゃいけない。だからこの挙動は、三値論理的にどうかとおもうが）


## expireAfterSeconds

TTL .. time to live.

* [MongoDB TTLのexpireAfterSecondsを変更する](https://akataworks.hatenadiary.jp/entry/2017/10/17/165123)
* [MongodbでTTLを設定する | Qiita](https://qiita.com/taka4sato/items/19f2fcc9184e9a0c8791)
* [Expire Data from Collections by Setting TTL -- MongoDB Manual](https://docs.mongodb.com/manual/tutorial/expire-data/)
* [MongoDBでTTL - Carpe Diem](https://christina04.hatenablog.com/entry/2015/02/02/205503)

引用: [MongoDB TTLのexpireAfterSecondsを変更する](https://akataworks.hatenadiary.jp/entry/2017/10/17/165123)

> MongoDBにはドキュメントサイズで削除するキャップ付きコレクションと有効
> 期限で削除するTTLインデックスがあります
>
> TTLインデックスの有効期限であるexpireAfterSecondsを変更する方法には...


## field/direction :: ascending | descending

direction は 昇順(ascending) 降順(descending)  になってる。
インデックスのソート順

インデックスに順番があるのは、仕組みとしては C++ のstd::map を意識すると理解しやすい？
ユーザーとしては何も考えずに取り出したときの順番として意味を持つのかな？
