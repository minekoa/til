# スペルと文法のチェック

## スペルチェッカー

### ispell-mode で Aspel を使う

[Emacsでスペルチェック|計算物理屋の研究備忘録](http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543)

Aspellもともと入っているらしい。

```
$ aspell
使い方: aspell [オプション] <コマンド>
<コマンド> は次のうちの１つです:
  -?|usage         短い使用方法の表示
  help             詳しい使用方法の表示
  -c|check <ファイル>  ファイルをチェック
  -a|pipe          "ispell -a" 互換モード
  [dump] config    現在のすべての設定を標準出力に表示する
  config <key>     指定したオプションの現在の値を表示する
  [dump] dicts | filters | modes
    利用可能な辞書/フィルタ/フィルタモード の一覧を表示する
[オプション] には以下のものがあります:
  --encoding=<str>            予測されるデータのエンコーディング
  --mode=<str>                フィルタモード
  -l,--lang=<str>             言語のコード
  -d,--master=<str>           使用する主辞書のベース名
  --sug-mode=<str>            提案モード
```

おぉ、入っとる！

ただ、これは汎用の言語チェッカなので、環境変数で日本語つかっていたりすると
日本語の辞書を探しに行ってしまうみたい。それを防ぐため
~/.aspell.conf に以下のように記述する

```
lang en_US
```

さて、試しにやってみよう

```
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
```

M-x ispell-buffer でバッファ全体、 M-x ispell-regionでリージョン内

fly-spell にしちゃうと、プログラミング言語の構文がひっかかってしまいそうなので
そこはやめとく。

## 文法チェッカー

文法チェッカーはどうしようかな。

[英文法チェックソフトのまとめ|rcmdnk's blog](https://rcmdnk.com/blog/2016/02/22/computer-english/)

* 有償でいいなら [Grammarly](https://www.grammarly.com/)
* 無償の中からなら [Ginger](http://www.getginger.jp/)
* オープンソースなら [LanguageTool](https://www.languagetool.org/)

langtool は langool.el というがある

## language-tool (途中諦め)

http://5zalt.hatenablog.com/entry/2014/11/13/195340 で

> ```
> $ brew install languagetool
> ```

とかしてたんで、apt にあるかなー、とおもってやってみた。

```
$ sudo apt-get install languagetool

No apt package "languagetool", but there is a snap with that name.
Try "snap install languagetool"
```

snap なんぞ？っと思ったら、canonical が提唱する新しいパッケージ。
dockerみたいなコンテナに閉じ込めるのと(なのでディストリやディストリバージョン非依存)、
自動アップグレードが特徴（Androidアプリみたいになってる）

* [Ubuntu Weekly Recipe 第515回　Ubuntu 18.04 LTSとSnapパッケージ](https://gihyo.jp/admin/serial/01/ubuntu-recipe/0515)
* [Linuxの新しいパッケージフォーマット、Ubuntu生まれのSnapsは、小アプリケーション群のためのコンテナのようだ | techcrunch](https://jp.techcrunch.com/2016/06/15/20160614ubuntus-container-style-snap-app-packages-now-work-on-other-linux-distributions/)
* [Ubuntu 18.04にsnapを利用してVisual Studio Codeをインストールする](https://qiita.com/ykmchd/items/707e5460f739c0c775d2)

> snapとは、アーカイブフォーマットです。snapdというツールをサポートするLinuxディストリビューションで動作可能です。snapの中でライブラリが完結するため、システムに依存せずにインストールや更新が可能です

ほえほえ。

さて、languagetool をi入れてみる

```
$ sudo snap install languagetool
languagetool 4.4-snap2 from vasilisc (vs) installed
```

```
$ languagetool
```

ほげー、GUIだ

で入れてみて気がついたのだけれども、langtool-modeは、jarファイルのパスを求めるのだけれども
この入れ方するとそれができるのかよくわかんない。（というかjarどこにあるの？）

というわけで、emacsから使う・・のというミッションは未完なのだけれども、
時間使いすぎたのでタイムアップ。残念。
