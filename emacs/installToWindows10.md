# Windows 10 に Emacs をインストールする

こちらを参考にしました。

[GNU Emacs for Windows再入門](https://emacs-jp.github.io/tips/emacs-for-windows)

## scoop をつかって emacsをインストールする

PowerShellを起動する

```
> Set-ExecutionPolicy RemoteSigned -scope CurrentUser
> Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')
> scoop install git
> scoop bucket add extras
> scoop install gow
> scoop install emacs
```

## emacs.d

```
> cd AppData\Roaming
> git clone https://github.com/minekoa/.emacs.d.git
```

`.emacs.d/init.el` で、
Mozc関連をコメントアウトする

Alt-` でアラートがなるのと、Windowsのアラートは耳にさわるので以下を追加

```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Windows Customize)

(setq visible-bell t)
(global-set-key [M-kanji] 'ignore)
```

## Fonts

Ricky をいれてみた

https://github.com/mzyy94/RictyDiminished-for-Powerline

.emacs.d を買えてもいいけれど、GUIでサクッと、[Options] > [Set Default Font..] で変更。 Ricty Diminished Discord を設定。
