# Markdown執筆環境の構築

書くのはいいのですけれども、プレビューがしたい


## 最初はChrome拡張のMarkdownPreviewPlusを使おうとするも失敗

最初はChrome拡張のMarkdownPreviewPlusを使おうとしたのですが、
どうにも文字化けが直せずに失敗。なぜ。なぜ。


## Emacs の Markdown Preview を使う

`markdown-mode` の `M-x markdown-previw` とか、`C-c C-c v` とかを使えばいいじゃんということで、まずは makdown to html 変換のよさ気なツールをさがします。


```
$ npm install marked -g
```

で、`.emacs` とかに

```
(setq markdown-command "marked")
```

してあげれば、プレビューはきどうするのですが、なぜか google-chromeが起動しません。


