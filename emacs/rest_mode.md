# reST モード

## ヘッダマークアップの自動化

レビュー指摘で、

> 通例では、メインのテキスト末尾に合わせる感じ

とのコメント。ymirlib/doc をみると、区切り書式のイメージではなく、
アンダーラインのイメージでマークアップしている。

（こういうのがあるから reST いけてないんだよなー。markdownのほうが
テキストファイルでのマークアップが直感的だし、インデントとかに意味持たせないようにしているし
そういうところの快適さを無視したから普及しないんだよなって個人的には思うよ）

Emacs のモードでいい感じに助けてくれそうな項目だ。

* [reStructuredText(reST)をEmacsで書く際のまとめ](https://ymotongpoo.hatenablog.com/entry/20101106/1289007403)
* [reSt を rst-mode でやるためのメモ](https://cortyuming.hateblo.jp/entry/20081231/p1)
こんなん見つけた。

`C-=` で見出しつけるね。何回か押すと見出しレベルが切り替わる 1007


## ついでにチートシート

| Keybinds            | Description                                                    | note.. |
|---------------------|----------------------------------------------------------------|--------|
| C-= (rst-adjust)    | 押すたびに状況によって見出しが == とか ~~ に切り替わる （C-- C-=で逆）| ooO(C--ってどう入れるんだろ) |
| C-c C-t (rst-toc)   | 見出し一覧                                                       |        |
| C-c 1 (rst-compile) | reStで形成されたhtmlを作る                                        |
| C-c 5               | Firefoxで表示(slides.js)                                        | ブラウザ上で "t"で見た目（slideshowとアウトラインモード？）に切り替わる, "c"キー押すか、カーソルを右下もっていくと操作ボタン表示 |

## ヘッダレベルのカスタマイズ

`C-=` で出てくるヘッダマークアップの出現順とか、そもそも社内規約的に出現しないやつがいるとか。
どこでカスタマイズするのやら。

[rst.el](http://docutils.sourceforge.net/tools/editors/emacs/rst.el) を見ると、
`rst-preferred-adornments` というので設定されている。

```elisp
(defcustom rst-preferred-adornments '((?= over-and-under 1)
                                      (?= simple 0)
                                      (?- simple 0)
                                      (?~ simple 0)
                                      (?+ simple 0)
                                      (?` simple 0)
                                      (?# simple 0)
                                      (?@ simple 0))
```

これでカスタマイズ可能っぽい。`defcustom` ってどうするのがいいんだっけ？

[defcustomで定義された変数はsetqではなくcustom-set-variablesで設定すべき理由](http://kawamuray.hatenablog.com/entry/2013/11/03/180543)

いろいろ書いてある。読むべし、だけど、ここでは結論から書くと

```elisp
(defcustom foo-variable nil
  "Foo variable")
```

に対しては、

```elisp
(custom-set-variables '(foo-variable "hoge"))
```
みたいな感じで `custom-set-values` で設定すればいい。
(`setq`と違い、`require` があとから行われても大丈夫)

今回は、社内のドキュメント規約に沿ったレベリング書式の設定を行った。

```elisp
(custom-set-variables '(rst-preferred-adornments
                        `((?= simple 0)    ; 第1節
                          (?- simple 0)    ; 1.1 subsection
                          (?^ simple 0)    ; 1.1.1 subsubsection
                          (?\" simple 0)   ; 段落
                          (?~ simple 0)
                          (?+ simple 0)
                          (?` simple 0)
                          (?# simple 0)
                          (?@ simple 0))))
```

## ヘッダのカラーテーマ

`frame-background-mode ` をみて
ヘッダのマークアップカラーを調整してるみたい。でもこれ、 nil なんだよね


```
(setq frame-background-mode dark)
```
するのもかっこ悪いので、なんかいい方法ないかなってぐぐったら

[Emacs: background-mode | piyolian](http://piyolian.blogspot.com/2011/12/frame-background-mode-rst-mode-rst.html)

で

```
(setq frame-background-mode (frame-parameter nil 'background-mode))
```

とすればいいよとあった。確かに。
