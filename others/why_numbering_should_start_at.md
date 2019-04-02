## なぜナンバリングは●からはじめるべきなのか？

* [Why numbering should start at zero](http://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html)
* [Why numbering should start at one](https://nintil.com/2017/09/16/why-numbering-should-start-at-one/)


```
     ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
    1│ 2│ 3│ 4│ 5│ 6│ 7│ 8│ 9│10│11│12│13
     └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
a)     ●------------------------------------------○
b) ○-------------------------------------------●
c)     ●---------------------------------------●
d) ○----------------------------------------------○
```

```
a)     [2, 13)
b)     (1, 12]
c)     [2, 13]
d)     (1, 13)
```
1 <= i <= 1

どの表現がいいかな？

* `1 <` について、
    * もし最小の自然数から始まるケースだと、(最小-1)  という表現になり醜い
    * ここは閉区間 `2 <=` 表記だろう
* `< 12` について
    * 空のシーケンス を表現するのつらくない？
    * `1 <= i <= 1` でも 1が残っちゃうよ

なので、[2,13) 表現が最高なのだ！

(以上がダイクストラの主張)


## ツイートへの返事

こんにちは! はじめまして! よろしく!

### Introduction:

Yesterday, New Japanese imperial period name "令和(Rei-wa: "令" means somtihg about difference of "lady" and "girl", like good? beatiful? cool? ..umm. "和" means harmony? We are all good friands? about that)" is released.
数え年(means Counting-year) is way of age reckoning tradisionally used in japan.
This is starting age1 and updated on the New Year's Day.

Some japanese C++ guru tweeted about: How to explain it to a programmer,
You are good to say "数え年 is the same as 1based index" but you should escape from him/her because the programmer to be in agony.

But, I'm Smalltalker(and Erlanger). I tweet that I like 1based index.
The guru said thad 0based index is the best way! Dijkstra too said.

"Gnunu.., I understand 0based index is the good way one. but.."
I googled at "Why numbering should start at one".

## I think:

An offset-value and a numbering-value(counting label) have difference of 1.
It called ヤマタノオロチ問題(Probrem of 8mata-no-orochi) in Japan.
This オロチ(gient snake) has 8heads but his name is "8 branch-points Orochi".
All Japanese people learnd ツッコミ by this story in childhood: "Realy, should named ナナマタノオロチ!"

I think 0based index's power from numbring-value equal offset-value.
It's very conbinience because ex) "Here is the numbering-value? the other is offset? hummm" is nothing.

If numbering start ad 0 canceled this difference
because 0 is additive identity and addition has commutative property.
It's a nice trick.

But, This trick makes "numbering(-value)" non-"counting(-label)".
4ex; "count 0" means nothing, but 0th is ... what's???
It' bad effect by this trick, I think.

I intended to write a blog entry about this,
but I found your entory and it's so gread good,
my motivation is gone.
(Ofen this phenomenon called "もう全部あいつ一人でいいんじゃないかな" in Japan)

... The above is the translation of the previous tweet.
It has become so long because I'm not good at English very very not good,
but This is the same as previos tweet.


## 日本語原案

コンニチワ！ハジメマシテ！ヨロシク！

### はじまり

昨日、新たな元号「令和」が公開されました。
それにちなみ、あるC++のグルが「数え年をプログラマに教えるには1-basedインデックスと同じといえばいいよ
。でもそれを言うと悶絶するので逃げるべきだね」というジョークをツイートしました。
（C++のグルが文法についてこんなこというのは、昨日がエイプリルフールだったからかもしれません）

でも私はSmalltalker（兼 Erlangつかい) なので、
わたしは1basedインデックスすきだな、といったところ
グルは「いやいや、0-basedのほうが優れてるんだよ、ダイクストラもそう行ってる」といいました。

わたしは、「うううむぅ、よいのはわかるけど、でもねー」と思い
"Why numbering should start at one" とぐぐったのでした。

### こんなイメージ

オフセット量と数え上げラベルに一つ差がでるのは、
マタノオロチ問題と言われるくらい、普遍的な古くからの混乱要因です。

私は、0basedインデックスの力の源は、数え上げ量とオフセット量が
同じになるということにあると思います。
これは数え上げ量？あっちはオフセット量？と
いちいち考えないでもうまく行くのはとても便利です。

0は加法の単位元で、加法は交換則が成り立つから、0から始めれば、
"0 +1" -> "1 +0" -> "1" と、"+1" 一つ分が相殺されて
オフセット量と数え上げ量の差1 が消える、というトリックが成り立ちます。
とても上手い手です。

だた、このトリックは悪い副作用が無いわけではありません。
だってもうそれは「数え上げ（のラベル）」でなくなってしまいます。
たとえば、0個は何もないことを示しますが、じゃあ 0番目ってなんなのん？

わたしはこれを「n番目」という概念のニューバージョンとするのは
なんだかなぁ、と思うし、その気持ちを記事に書こうかな、とおもったのだが
とても良い記事をみつけたので、もう全部あいつでいいんじゃないかなって思いました。


・・・というのが、先のツイートの中身です。
英語苦手なので長くなってしまいました。またね。
