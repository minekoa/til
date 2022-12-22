
# お声

## ボイチェン

* [今の環境の話とか使ってる機材の話](https://magrona.fanbox.cc/posts/828702)
    * まぐろなちゃん
    * ぶっちゃけ ASIO対応オーディオインターフェイスでほぼ遅延なくなる作戦を踏襲させてもらってます

* [わたしのボイチェンの話](https://note.com/gasoko0150/n/n91e720d9b333)
    * Cakewalkは試してみたいが多分使いこなせない

* [](https://togetter.com/li/1262353)

* [ゼロから始めるボイチェン ～古い情報で損をしない為に～](https://note.com/hapikle_sour/n/n6dd231c2222d)


### マイク

[マランツ『MPM-1000』1年使用レビュー【性能十分な激安コンデンサーマイクだが、付属品には少し注意】](https://maglog.tokyo/mpm-1000-review/)

最初はSHAREのダイナミックマイクを買おうと思ってたけれども、あまりに安い（5,832円）コンデンサマイクなので
こちらを買ってしまった（スタンド、ショックマウント、ウインドジャマー XLBケーブルと一通りついてきての
この価格はダイナミックマイクよりやすかった）


コンデンサマイクは扱いがセンシティブ（特に湿気）と聞くけれど、

> コンデンサーマイクはきちんと管理しないと壊れてしまうという話がよくありますが、とりあえず僕のMPM-1000はデスクに出しっぱなしでも元気に音を拾ってくれます。

とあったので。
（ページ閉じてキーワード忘れちゃったけれど、
コンデンサーマイクにも２つの方式があって、
湿気とかにそんなにシビアじゃない方式もあるみたい）

https://twitter.com/HinataTosaki/status/1035666355939291136

> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> 配信機材といえば大事なのはマイクだけど、皆の憧れコンデンサーマイクのお話。コンデンサーマイクって保管が面倒くさいって言われてるけど、面倒くさいのはDCバイアス型でバックエレクトレット型はダイナミックマイクみたいに基本出しっぱなしでも問題なかったりします。
>
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> DCバイアス型はひなたも持ってるけど、使ったら外して防湿庫に入れないといけないから大変面倒くさいです。壊れやすいので落としたり出来ない上に結構アレなお値段がするので宅録環境でしか使っていません。
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> ひなたが配信環境で使ってるのはバックエレクトレット型のやつで、オーディオテクニカのAT2035ってのを使ってます。もちろん出しっぱなし。上から布かぶせてるくらいかなー。
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> コンデンサーマイクにはファントム(ファンタム)電源が必要なので、オーディオインターフェイスが必要になります。音質的にはダイナミックマイクはさらっとしてるけど、コンデンサーマイクはしっとりしてる感じかな。しっとり声が良い人はコンデンサーマイクがおすすめ。
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> ただあんまり安いコンデンサーマイクだとあんまりよくなかったりするので、できればオーディオテクニカだとAT2020かひなたが使ってるAT2035(ショックマウントついてるからおすすめ)あたりから上のやつが良いです。

* DCバイアス型 ..ヤバい。防湿庫へしまえ
* バックエレクトレット型 ..基本出しっぱ

らしい。

## VSThost

DAW全然わからん、ということで[VSTHost](https://www.hermannseib.com/english/vsthost.htm)を使う。
一つを除きとてもかんたんだった。


こちらを参考につくった

[声が低い人のためのVSTHostを使ったボイスチェンジャーの作り方](http://blog.vrai.jp/article/469327817.html)

たすかりんぐ

[VSTHostを使ったリアルタイムでの音声加工と最適な設定](http://www.brillyaht.com/archives/121)
バッファとか。


[VSTHostの設定と操作](https://yppts.adam.ne.jp/music/isogi/vsthost.html)

詳しい・・。よくわかんない。MONOをエンジンで解決・・はここみて、あ、できるんだって気がついた。



[最近の私的ボイチェン事情](https://note.com/ruskbull/n/nb4f7c18d6982)

[恋声が合わなかった人にも試して欲しい、VSTプラグインでボイチェンする方法](https://ch.nicovideo.jp/torisoup/blomaga/ar1610846)
最後、REPPERにしたよってのってる

-----------------------------------------


問題解決については、以下のセクションにメモる

残る問題

* ASIOドライバーで接続するとInput/Output もASIOになっちゃう。
 （ヘッドホンないから出力出てるかは未確認だけどたぶんでてるんだろう）
 で、それを OBSに取り込む方法。
    * OBS に有志のASIOプラグインを入れた。これはASIOのInputをキャプチャーするので
      ループバックさせて、Ch2を取ればいいかなって思ったけど、
      VSTHostとOBSの間で、ASIOの排他確保が置きてダメだった。

なので今、お歌うたうの辛いくらいの遅延がある

Voice Meeter Banana を使うのが良いの？

メモ:

[VoiceMeeter Banana備忘録](https://frailleaves.com/broadcast/vmbanana/)
[恋声+Voice Meeter Bananaの環境構築とVSTプラグイン](https://fuyunoyukihana.hatenablog.com/entry/2018/11/20/211749)

こことかどんぴしゃっぽい？(ちゃんと読んでないけど）

[AG03でケロケロ配信しよう！(フリーソフトを使って)](http://randrandr.blog.fc2.com/blog-entry-42.html)

[Windowsユーザにオススメの万能仮想ミキサー「VoiceMeeter Banana」が凄い](https://av.watch.impress.co.jp/docs/series/dal/1255935.html)


[Cubase & OBS を使ってボイチェン配信する方法](https://note.com/rhnnsi/n/n478aaa3544d7)

### UR22C　VSThost チャネル片方だけ問題

1. ループバック作戦
    * [UR22C のマイク入力を LとR両方のチャンネルに録音する](https://www.ipentec.com/document/hardware-steinberg-ur22c-record-microphone-input-to-l-and-r-channel)
2. VSThostのEngineメニューで両方ともinput1にしちゃう


### VSTプラグイン

* [graillon2](https://www.auburnsounds.com/products/Graillon.html)
* [rovee](https://www.g200kg.com/jp/software/rovee.html)


[無料で使えるプラグイン特集 / エフェクト編](https://chanoma.realfreedom.jp/archives/5671)


### イコライザ

https://togetter.com/li/1262353

> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> あと、バ美肉で困るのは自分の声をどうやって女性の声に近づけるかってことなんだけど、イコライザである程度までは持って行けるのでボイスチェンジャープラグインの後ろにイコライザプラグインを入れて不要な音をカットしていくと良いかなぁ。
>
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> 個人差あるのできまった設定っていうのはないんだけど、目安としてざっくり書くと、200Hz-250Hzが男性っぽさの帯域になります。なのでとりあえずローカットで200Hzくらいから下をバッサリやっちゃうと良いかも。
>
> 戸咲ひなた☀️ネコミミ系VTuber @HinataTosaki
> あとは500Hzから800Hzあたりに声の芯があるのでそこを持ち上げて、1KHzから2KHzあたりに声の艶、2K-3KHzは声の抜けがよくなるんでその辺も持ち上げる。ただし3KHz付近に歯擦音の帯域があるので、さしすせそが耳に刺さらないように。上げすぎはダメ。持ち上げるのは最大で2.5dbくらいまでが良いです。

### コンプレッサーの使いかた

音が小さいと苦情を頂いたので（自分でも小さいな、と思ったり）、ググる。
（普通にゲインを上げるとノイズが大きくなったり割れたり、EQ書けてたりすると普通に落ちたりする）

* [音圧の上げ方（音を大きくする方法）](https://www.dtmfb.com/technique/onatsu/)

なるほど、コンプレッサをつかうのか

[コンプレッサーの仕組みと基本的な使い方](https://chirico-music.net/compressor/)

## DAW

[VSTとDAW](https://saeokuri.hatenablog.com/entry/2019/12/17/041557)

チャレンジかなぁ

## 喋りにくさ

配信時の滑舌の悪さ

普段からこんななのかなー…と地味にショックだったのですが

たまたまAmazonの セールで安かった OneOdio Pro10 を勝って オーディオIFにつないでみたら

…なんか喋りやすい

これまで USBヘッドセットでモニタしてたのですが、ひょっとして遅延で言語野混乱ってやつです？
