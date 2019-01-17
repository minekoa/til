# 行末の空白を削除する

コードレビューで結構指摘されたので。

Emacs ならば、

```
M-x delete-trailing-whitespace
```

で消せる。
毎回手動で実行するのはアレなので、

```
(add-hook 'before-save-hook 'delete-trailing-whitespace)
```

を `.emacs` に書けば良い。

## 余談

> ちなみにtrailという単語を気になって調べたのですが、「たなびく」って意味でした。
>
> [無駄な行末の空白を削除する(Emacs Advent Calendar jp:2010)](http://tototoshi.hatenablog.com/entry/20101202/1291289625)


## 参考

* [無駄な行末の空白を削除する(Emacs Advent Calendar jp:2010)](http://tototoshi.hatenablog.com/entry/20101202/1291289625)
* [Markdown-modeのみ、ファイル保存時の行末の空白自動削除を無効にする | Qiita](https://qiita.com/UFO/items/4656d0740a67cca289db)

Markdownは末尾空白に意味があるので、

```
(defvar delete-trailing-whitespece-before-save t)
(make-variable-buffer-local 'delete-trailing-whitespece-before-save)
(advice-add 'delete-trailing-whitespace :before-while
            (lambda () delete-trailing-whitespece-before-save))
```
なんかで、無効にするのがよいかもかも。

