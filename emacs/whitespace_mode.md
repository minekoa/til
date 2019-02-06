# jaspace-mode から whitespace-mode への移行

## プロローグ（動機）

emacs-mozc糞重い問題調べてるけどだめだな、変換候補表示が重いのかな、という試みの一貫として
mozc-popup を入れてみた。

[Mozcの情報まとめ的な](https://qiita.com/ballforest/items/063a7fb4d0661c2bcf07)

・・・重いなぁ、これ…と思っていたのですが、
バッファーによって重さが違う。よくよく見てみれば jaspace-mode が有効な場合、
大変重いことがわかりました。

相性が最悪だなぁ。…まてよ、というか普段emacs-mozc糞重いのも実はjaspace-modeのせい？

というわけで、これを使わないことにしました。

## whitespace-mode

とはいえ、改行文字の表示、タブ文字の表示、全角スペースの表示は
無いと困ります。そこで、何か代替がないかな、とおもって探してみれば
今はだいたい whitespace-mode 一強なのですね。
（最初から入っているので、特にパッケージインストールとかしなくてよい)

というわけで、こちら

* [whitespace-modeの設定](http://syohex.hatenablog.com/entry/20110119/1295450495)
* [emacsで改行位置に記号を表示する | beizのノート](https://beiznotes.org/201407101404983895-2/)
* [Emacs Lisp の whitespace.el の設定でかなりはまったのでメモ (Emacs23) | スケーラブルな医者になりたい in Tokyo (Unofficial)](http://d.hatena.ne.jp/t_ume_tky/20120906/1346943019)

を参考に設定しました。最悪背景色やアンダーラインで妥協しようと思ってたのですが、
だいたい同じことできるのですね。


ちなみに最後の記事に書いてあった

> １．white-space-style
>
> ここに列挙しておかないと、設定が有効にならない。
> newline と newline-markの違いとかよくわからないけど、とりあえず記載した。
>
> 違いがわかる方がいれば教えてください。m(＿＿)m

ですが、white-space-style に列挙する newline と newline-mark の違いは
前者は色付け、後者はグリフの置き換えです。


今の所の設定はこんな感じになりました。

```
(require 'whitespace)

(setq whitespace-style '(face tabs tab-mark spaces space-mark newline newline-mark))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (newline-mark ?\n    [?\xAB ?\n] [?$ ?\n])    ; end-of-line
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-tab "#555588")
(set-face-background 'whitespace-tab 'nil)
(set-face-foreground 'whitespace-space "#555588")
(set-face-background 'whitespace-space 'nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-newline "#555588")
(set-face-background 'whitespace-newline 'nil)
(global-whitespace-mode 1)
(global-set-key (kbd "C-x w") 'global-whitespace-mode)
```

あとは各メジャーモードの ``add-hook`` を jaspace-mode から置き換えてあげて
おしまい。

## 既知の問題

whitespace-mode の既知の問題として、タブを文字表示するとインデントが崩れるときがあるみたいですね。
