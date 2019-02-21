# RELAX NG について

リラクシングと読むらしい。…よめるか！
あと NG は New Generation ということらしい。

> XMLのスキーマ言語の一つである

* [RELAX NG 仕様書 - Committee Specification 3 December 2001](http://www.asahi-net.or.jp/~eb2m-mrt/relaxngjis/jaspec-20011203.html)
* [RELAX NG 入門 - 部会承認済み仕様書 (2001年8月10日)](http://www.kohsuke.org/relaxng/tutorial.ja.html)

多分ちゃんと理解するにはよんだほうがいいのだろうけれど、
そんなに時間ないので、今日はさようなら。


## 特徴

* [XMLの論考: RELAX NGによる逆襲 第1回](https://www.ibm.com/developerworks/jp/xml/library/x-matters25/index.html)

サブタイトル「W3CのXML Schemaを打ち負かす」が物騒ですね

> ところが、1か月ほど前からRELAX NGのことを真剣に調べるようになりました。
> 多くの読者の皆さんと同じように、私自身もこの代替スキーマ言語については
> 前から聞いていましたが、基本的なところは同じで、若干のスペルの違いがあるだけだろうと
> 思い込んでいました。

> ところが、その思い込みは大間違いでした。
> RELAX NGは、ほとんどすべての点で、W3CのXML Schemaよりも、
> さらにはDTDよりも優れているのです。現に、RELAX NGが順序なしの
> (または半順序付きの) 内容モデルをサポートできるという点は、OOPのデー
> タ型の意味体系とXML要素の直線的な並べ方とがどうしてもなじまないとい
> う、私のかねてからの悩みを解消してくれるものでもあったわけです。

* 意味体系モデル
    * RELAX NGスキーマで記述するのは、数量定義、順序、選択肢からなる「パターン」
    * 順序なしコレクションのパターンもサポート! (DTD, W3C XML Schema もノンサポ)

## 短縮記法ざくざく

だいたい良さを知ったかできるようになったところで、
内容に空いてWikipediaでさくっとわかったふりしておこう

[RELAX NG | Wikipedia](https://ja.wikipedia.org/wiki/RELAX_NG)

> RELAX NG で記述されたスキーマは、それ自身がXML文書である。

ふむ

> しかし RELAX NG では、スキーマをXML構文ではない簡潔な短縮構文 (Compact
> Syntax) で記述することもできる

今回キャッチアップしたいのは、この短縮構文。BNFとか代数データ型とか
そういう感じで読みやすく書きやすいと、個人的には思う。

> 短縮構文は、DTDの構文と多くの機能を共有している

> ```
> <?xml version="1.0" encoding="UTF-8"?>
> <book>
>   <page>これは1ページです。</page>
>   <page>これは2ページです。</page>
> </book>
> ```

は、短縮構文では

> ```
> element book
> {
>     element page { text }+
> }
> ```

または

> ```
> start = element book { page+ }
> page = element page { text }
> ```

と書き表せる。start, text は予約語かな？
→　XML Shchemaの予約語だな

`*` とか `?` とか `+` とかは、正規表現のあれと同じ

> RELAX NGで使用できる数量定義はDTDの場合と同じです。どんなパターンに
> も、<oneOrMore>、<zeroOrMore>、<optional> のいずれかによって条件を設
> 定できます。この書き方は、DTDの数量詞である+、*、? にそれぞれ相当し
> ます (これらの数量詞は、正規表現などでも使用されています)


> リスト2. 図書館利用者の短縮構文
>
> ```
> element patron {
>   element name { text }   &
>   element id-num { text } &
>   element book {
>     (attribute isbn { text } |
>      attribute title { text } )
>   }*
> }
> ```

ここで注目すべきは book の attribute 。 `isbn | title` と記載されているが、

> (DTDとW3CのXML Schemaの場合、属性は必須か省略可能かのいずれかであり、
> 属性の相互関係を記述することはできません)

これは RELAX NG の強み。

すこしわからないのは `&`

`&` はelement連結のよう。choise でかけても連結できるので
リストと考えるのはどうなんだろう。順序はあるのかな？
`,`カンマとの意味の違いも知りたい。

ちょちょっと調べた限りではフックしなかった。
あやふや情報だけれども、カンマは順番ありっぽいね。

少し調べてみた。

[8.Interleaving | RELAX NG Compact Syntax Tutorial](https://relaxng.org/compact-tutorial-20030326.html#id2815185)

`&` は Interleave connector と呼ばれてる。

引用: [8.Interleaving | RELAX NG Compact Syntax Tutorial](https://relaxng.org/compact-tutorial-20030326.html#id2815185)

> In addition to the `,` and `|` connectors, RELAX NG provides the `&`
> connector. This is useful when child elements are allowed in any
> order.
>
> ```
> element addressBook {
>   element card {
>     element name { text }
>     & element email { text }
>   }*
> }
> ```
>
> The & connector is called the interleave connector because of how it
> works with patterns that match more than one element.

…うん。だいたいわかった

## あとでよむ

* [RELAX NG Compact Syntax Tutotial を読む（1）](http://d.hatena.ne.jp/kawacho/20080218/1203296350)
* [RELAX NG Compact Syntax Tutorial | Working Draft 26 March 2003](https://relaxng.org/compact-tutorial-20030326.html)
