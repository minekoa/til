# markdown-mode のリスト記法の自動インデントを改善する

markdown モードのリストの自動インデントがうざいです。問題は２つ

* 改行するとインデントクリアされてしまう、これは望む挙動ではない
* 1インデント4スペースにしたい（markdownパーザ側がインデントでないと判断することがある）

## 改行するとインデントクリアされてしまう問題

これは流石にみんな困ってる。どうやら Emacs 24.5 から入り込んだ
不具合(?)みたい。

* [Emacsを24.5に上げたらmarkdown-modeのインデントがおかしくなった](https://blog.shibayu36.org/entry/2015/08/04/190956)
* http://www.nofuture.tv/diary/20141114.html

Emacs 24.5 で electric-indent-modeが新設されて、
その結果おかしくなっていた、ということらしい。

これを抑制するためのハック

```
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))
```


## あと、なにげにリストの インデント量が2なのはうざい

markdownのnested list は、4インデントが一般的なのに、なんで我が道をいくのかね？…と思ったら


[Indent Code and Lists by 4 Spaces in Emacs Markdown Mode](https://christiantietze.de/posts/2018/05/emacs-always-indent-tabs/)

> Turns out you're supposed to turn this off for text-mode (a parent mode of Markdown).
> See the great Emacs indentation tutorial for a good reference.
>
> Using hooks, here's what I am now using to indent by 4 spaces
> instead of adhering to tab stops:

>
> ```
> (add-hook 'text-mode-hook
>           '(lambda ()
>              (setq indent-tabs-mode nil)
>              (setq tab-width 4)))
> ```

えっ、text-modeなの？そっちから持ってきてたのかー。

設定すると一応聞いている？みたい？
（最初のインデントは2(多分文字頭に合わせに行っている）だけどその後4)


ココらへん困るのは、実は github とかpandocとかでインデントネスト聞いてないじゃーんなときなので、
ならgfmモードをつかうべきなのかもしれないなあ、とも思ったり。
