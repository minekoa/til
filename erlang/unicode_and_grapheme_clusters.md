# ユニコードと書紀素クラスタ

## 書記素クラスタとは

乱暴にいえば、複数のコードポイントで「一文字」を表すようなものを考慮した、文字の単位。
ハングルとか、Macな 濁音半濁音とか。

* [Unicodeのgrapheme cluster (書記素クラスタ) 2015/10/25](https://hydrocul.github.io/wiki/blog/2015/1025-unicode-grapheme-clusters.html)


> 人間が考える “文字” はUnicodeのコードポイント1つで表現できるとは限らない。 2つ以上のコードポイントを組み合わせて構成する文字もたくさんある。
>
> 例えば
>
> á
>
> という文字は、人間にとって1文字と認識するが、コードポイント2つ(U+0061 U+0301)で構成される。 (ただし、 “á” は U+00E1 という1つのコードポイントでも表現できる)
>
> Grapheme cluster の境界は、検索、正規表現でのマッチング処理、UIなどで重要である。
>


いわゆる文字数カウントや、カーソル、削除・編集制御は、コードポイント由来の「一文字」で処理すると


### グリフと grapheme 、grapheme cluster

グリフは「文字」だけど字形を伴う。
なので、「a」とか「そ」とか、文字の書き方により形が変わるのを **同じ文字だけど** 「グリフが違う」と表現する。

grapheme は,
この「同じ文字だけど」で言うところの文字で、より抽象的な「文字」。


grapheme cluster は、一つの grapheme を表すコードポイントの列のこと。
（grapheme の cluseter じゃなくって、grapheme を示す コードポイントの cluster)



## Erlang での対応

たとえば、コードポイントと書記素クラスタ数の違いは以下のように現れる

```erlang
1> Value = <<"á"/utf8>>.
<<97,204,129>>
2> string:length(Value).
1
3> unicode:characters_to_list(Value).
[97,769]
4> length(unicode:characters_to_list(Value)).
2
```

このようにErlangで書記素クラスタ単位で何かしたい場合は、
概ねstringモジュールを使えばいい

[Erlang - string](https://erlang.org/doc/man/string.html)


> ## `length(String :: unicode:chardata()) -> integer() >= 0`
>
> Returns the number of grapheme clusters in `String`.


他には、こちらも面白かった

[Erlang & Unicode -- 10 min guide for a quick, incomplete understanding](https://ferd.ca/static/lightning-unicode-erl.pdf) (PDF)
