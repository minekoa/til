# PBKDF2-HMAC-SHA-512って何？

## 発端

パスワードのダイジェスト認証を実装する必要があったのだけれど、その仕様にあった
`PBKDF2-HMAC-SHA-512` ってなんぞや、ということ

まず端的に結論だけ言えば、Erlangでこれやるなら

* [basho/erlang-pbkdf2](https://github.com/basho/erlang-pbkdf2/blob/master/src/pbkdf2.erl)

を使えばいい。

以下は、わかんないまま使うのよくなくね？というお話

## まず用語の読み下し

`PBKDF2-HMAC-SHA-512` の意味がわからん

* `PBKDF2`  ..Password-Based Key Derivation Function 2
* `HMAC`    ..Hash-based Message Authentication Code
* `SHA-512` ..これは知っているけど割愛

`PBKDF2` と `HMAC` って併記するものなの？という疑問が湧く。

## 色々読む

以下のドキュメントツリーを読む

* [パスワードのハッシュに使うべきPBKDF2、Bcrypt、HMACの各言語実装一覧](https://matsuu.hatenablog.com/entry/20120106/1325852543)
    * [Are you sure SHA-1+salt is enough for passwords?](https://www.f-secure.com/weblog/archives/00002095.html)
    * [(日本語訳)](http://blog.f-secure.jp/archives/50564743.html)

まず参照先の日本語訳を読む

> 　以下から、こうしたスキームをいくつか選ぶことができる：
>
>  •  PBKDF2 http://en.wikipedia.org/wiki/PBKDF2
>  •  Bcrypt http://www.openwall.com/crypt/
>  •  HMAC http://en.wikipedia.org/wiki/HMAC


PBKDF2 と HMAC が出てくる。

* [HMAC | Wikipedia(en)](https://en.wikipedia.org/wiki/HMAC)
    * [(jp)](https://ja.wikipedia.org/wiki/HMAC)
* [PBKDF2 | Wikipedia(en)](https://en.wikipedia.org/wiki/PBKDF2)

## HMAC (Hash-based Message Authentication Code)

HMACは日本語あるので、まずHMACから見ていこう

> HMAC (Hash-based Message Authentication Code) とは、
> メッセージ認証符号 (MAC; Message Authentication Code) の一つであり、
> 秘密鍵とメッセージ（データ）とハッシュ関数をもとに計算される。
>
> 1997年2月、IBMのKrawczykらにより提唱され、RFC 2104として公開されている。
> また、FIPS PUB 198にも採用されている。

ふむん

日本語と英語で定義が若干ちがうが違う(日本語版は Key' がいない）が、大枠は

```
HMAC :: 秘密鍵 -> 認証対象メッセージ -> ダイジェスト
HMAC Key Msg ->
    let
       h   :: 繰り返しハッシュ関数
       Key :: 秘密鍵
       Msg :: 認証対象メッセージ
       ipad :: 0x3636363636.. というパターンで、かつ、長さが hのブロック長
       opad :: 0x5c5c5c5c5c.. というパターンで、かつ、長さが hのブロック長
       Key' = case Key of
                 larger then block size => h(Key);
                 otherwise              => Key
    in
        list:concat( [ h (Key' `xor` opad)
                     , h list:concat [(Key' `xor` ipad), Msg]
                     ]
                   )
```

この `h` が SHA256 ならば、`HMAC-SHA-256` というわけらしい。
(というか、秘密鍵どこから出てきた? Salt?）

> HMACにより算出された値はMAC値と呼ばれる。

ほへー。

## PBKDF2 (Password-Based Key Derivation Function 2: パスワードベースのキー派生関数2)

HMACがダイジェストを得るためのアルゴリズムにみえるならば、こいつはなんだ？

[HMAC-SHA-256 固定の PBKDF2 関数| Tociyuki::Diary](https://tociyuki.hatenablog.jp/entry/20150902/1441193334)

Javaでの実装が丁寧に解説されている。ふむん、これを読む... HMACどこ行った？


パスワード周りの用語（日本語）が知りたい

[【Java SE 8限定】安全なパスワードを生成する方法](https://www.casleyconsulting.co.jp/blog/engineer/153/)

> さて、ここまでの説明で見慣れない単語が２つあると思います。「PBKDF2」と「HMAC-SHA-256」ですね。
>
> 「PBKDF2」はPKCS(Public-Key Cryptography Standards、公開鍵暗号標準)の#5で定められているアルゴリズムで、
> 「Password-Based Key Derivation Function 2(パスワードベースのキー派生関数2)」の頭文字を取ったものです。
>
> 「HMAC-SHA-256」の「HMAC」は「Hash-based Message Authentication Code(ハッシュベースのメッセージ認証符号)」
> の頭文字を取ったもので、「SHA-256」がこのアルゴリズムで使用するハッシュ関数です。
>
> SOPHOS社のページには簡単な解説がありますが、「PBKDF2」はストレッチングのアルゴリズムで、
> 「HMAC-SHA-256」は「PBKDF2」の中で今回は使われているようです。

ストレッチング？これ、HMACの h（繰り返しハッシュ関数) の繰り返しでやることじゃないの？

> PBKDF2 関数は RFC 2898 の定義通りです。

[RFC 2898](https://tools.ietf.org/html/rfc2898#section-5.2)

定義をちゃんと読もう。

理解のためにHaskell風疑似コードで書き下し

```
PBKDF2 :: octed_string -> octed_string -> positive_integer -> positive_integer -> octet_string
PBKDF2 Passwd Salt IterationCnt dkLen ->
   !AssertMsg (dkLen > (2^32 -1) * hLen of) "derived key too long".
   let
      l = CEIL (dkLen / hLen)
      r = dkLen - (l -1) * hlen
   in
      DK = list:concat [ F(Passwd, Salt, IterationCnt, n) || n <- [1..l] ]

F Passwd Salt IterCnt I ->
  let
    PRF :: underlying pseudorandom function
    INT = \i -> ;; four-octet encoding of the integer i, most
                ;; significant octet first.
  in
    foldl( \0 _ ->
              PRF Passwd (list:concat [Salt, INT(I)])
           \_ Acc ->
              PRF Passwd Acc
         )
         [0..IterCnt].
```

この PRF 部分に `HMAC_SHA_256` を適用する…というのでいいのかな？
（ということは HMAC の `h` は、実は単なるSHA_256で、ストレッチングしていない落ち？）


・・・という、なんともあやふやな理解だが、今日はこのくらいで許してやろう。
