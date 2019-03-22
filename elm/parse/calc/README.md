
https://qiita.com/ogata-k/items/874811cf1aca41bfe88e

https://kazu-yamamoto.hatenablog.jp/entry/20080920/1221881130


[chainl と左再帰](https://kazu-yamamoto.hatenablog.jp/entry/20110127/1296098875)

> この問題を解決するために、Parsec では chainl というコンビネーターを
> 提供しているのだと思っていたが、山下さんや酒井さんにそれは間違いだと
> 教えて頂いた。chainl は左再帰を解決するのではなく、左再帰を除去した
> BNFを実装できるだけとのことだ。


[構文解析にまつわる小話たち | κeenのHappy Hacκing Blog](https://keens.github.io/slide/koubunkaisekiarekore/)


[とりとめのないパーサー談義](https://kazu-yamamoto.hatenablog.jp/entry/20081201/1228115457)

> 原文はこうです。
>
> > Parsec is an industrial strength, monadic parser combinator library for Haskell. It can parse context-sensitive, infinite look-ahead grammars but it performs best on predictive (LL[1]) grammars.
>
> 僕が訳すとこうなります。
>
> > Parsec は、Haskell の実用的なモナディク・パーサー・コンビネーター・ライブラリです。無限先読みの文脈依存文法を解析でき、LL(1)のときに最高の性能を発揮します。
>
> つまり、Parsec は、いくらでも後戻りでき、最悪すべてのパスを検査できるが、後戻りがない場合に一番効率がいいということですね。

まじか
