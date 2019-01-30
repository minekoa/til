# Emacs-mozc が糞重い（トライアル）

なんとかしたい。本当になんとかしたい。

## トライ1: Emacs に対して fcitx とか ibus が起動するのが原因では説

とりあえず、無効に設定してみた。`.Xresources` ファイルを以下の内容でつくる


~/.Xresources

```
emacs.useXIM:false
```

で、これを実行（再起動したくないなら）

```
$ xrdb -merge ~/.Xresources
```


しばらく様子見
