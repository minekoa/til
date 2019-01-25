# markdown-mode のコードブロックでフォントを変えない

正直うざい。

テキストエディタ上ではもとから等幅フォントつかってるので、
ここでフォント変えられて崩れても全く嬉しくない。

* [emacsのmarkdown-modeのcode blockでフォントを変えない](http://garaemon.hatenadiary.jp/entry/2018/04/15/070000)

```elisp
;; Do not change font in code block
(set-face-attribute 'markdown-code-face nil :inherit 'default)
```

ただ、これだと色も変わらなくなってしまうので、それはやだな。

* [emacsでmarkdown-modeのinline code blockに色を付ける](http://garaemon.hatenadiary.jp/entry/2018/04/18/180000)

```
(set-face-attribute 'markdown-code-face nil
                    :inherit 'default)
(set-face-attribute 'markdown-inline-code-face nil
                    :inherit 'default
                    :foreground (face-attribute font-lock-type-face :foreground))
```

私は、コードブロックも色を付けたいので、`markdown-code-face` にも `foreground` ... を記述。
