# Pixelbook Go の環境構築

## Crostini

`chrome://flags/` を使用。

* #crostini-use-buster-image
* #crostini-disk-resizing
* #crostini-username

を enabledに


## 日本語ロケールの設定

```
zoni@penguin:~/reps/til$ echo $LANG
en_US.UTF-8
```

```
zoni@penguin:~/reps/til$ locale -a
C
C.UTF-8
en_US.utf8
POSIX
```

設定

```
$ sudo dpkg-reconfigure locales
```

GUIがでるので、ja_JP.utf8 を追加し、デフォルトにする。

確認

```
oni@penguin:~/reps/til$ locale -a
C
C.UTF-8
en_US.utf8
ja_JP.utf8
POSIX
```

.bashrc に以下の二行を追加

```
export LANG=ja_JP.UTF-8
alias ll='ls -al --color=auto'
```

こちらも実行。

```
```

コンテナごと再起動（ChromeOSのランチャー経由でアプリ起動する場合.
どちらが効いたのかは不明。

## emacs インストール

普通に sudo apt install emacs する。（26.1が入る）
mozc-popup と elscreen を M-x package-list-packeges でインストール。

## VL-gothic のインストール

sudo apt install fonts-vlgothic

## git 補完の文字化け問題

```
git config --local core.quotepath false
```

## forti client

結論から言うと動作しない。

```
$ sudo openfortivpn xxx.xxx.xxx.xxx:xxxxx --username=xxxxxxx --trusted-cert xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
WARN:   Bad port in config file: "0".
VPN account password:
INFO:   Connected to gateway.
INFO:   Authenticated.
INFO:   Remote gateway has allocated a VPN.
ERROR:  read: Input/output error
INFO:   Cancelling threads...
ERROR:  pppd: The kernel does not support PPP, for example, the PPP kernel driver is not included or cannot be loaded.
INFO:   Terminated pppd.
INFO:   Closed connection to gateway.
INFO:   Logged out.
```

パーミッションの問題かもしれない。
(C302CA の crouton ubuntu で sudo つけないでやると、似たエラーがでるので。pppdのエラーだけちょっと違う。以下参照）

```
INFO:   Connected to gateway.
INFO:   Authenticated.
INFO:   Remote gateway has allocated a VPN.
ERROR:  read: Input/output error
INFO:   Cancelling threads...
ERROR:  pppd: Has detached, or otherwise the connection was successfully established and terminated at the peer;s request
INFO:   Terminated pppd.
INFO:   Closed connection to gateway.
INFO:   Logged out.
```
