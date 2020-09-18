# EzBox i3 に Ubuntu 20.04 LTS をいれる


## ディレクトリ名を英語に

```console
LANG=C xdg-user-dirs-gtk-update
```

## （なんとなく）xubuntu-desktop のインストール


## i3wm のインストール

```
sudo apt intall i3
```

## ディスプレイ設定

ubuntu では問題ないのですが、
xubuntu とかi3wm だとなんかが画面がちらつく（消えたりついたりする）のです。

こりはリフレッシュレートがだめだめちゃんだな、、ということで、
`xrander`で設定を確認。

```console
$ xrandr
Screen 0: minimum 320 x 200, current 2560 x 1080, maximum 16384 x 16384
HDMI-1 connected primary 2560x1080+0+0 (normal left inverted right x axis y axis) 798mm x 334mm
   2560x1080     59.98*+  74.99    60.00    59.94    50.00
   3840x2160     30.00    25.00    24.00    29.97    23.98
   2560x1440     59.95
   1920x1080     74.99    60.00    50.00    59.94
   1680x1050     59.88
   1600x900      60.00
   1280x1024     75.02    60.02
   1280x800      59.91
   1152x864      75.00    59.97
   1280x720      60.00    50.00    59.94
   1024x768      75.03    60.00
   832x624       74.55
   800x600       75.00    60.32
   720x576       50.00
   720x480       60.00    59.94
   640x480       75.00    60.00    59.94
HDMI-2 disconnected (normal left inverted right x axis y axis)
```

つなげているモニターは LG 29WN600-W なので、スペックとしては
2560x1080 50Hz なので、リフレッシュレートを50Hzに設定してあげましょう。

```console
zoni@zoni-EZbox:~$ xrandr -r 50
```

うまくいったら、~/.profile に永続化。

## Emacsのセットアップ

```
$ sudo apt install emacs
$ sudo apt install emacs-mozc-bin
$ sudo apt install fonts-vlgothic
```

github から 秘伝の .emacs.d を持ってきます。

```console
$ cd ~
$ git clone git@github.com:minekoa/.emacs.d.git
```

その後Emacs上で

```
M-x list-packages
```

で表示されるパッケージの中から

* mozc
* mozc-popup
* elscreen

## 日本語入力可能にする (i3wm)

```
$ sudo apt install fcitx-mozc
```

~/.config/i3/config に

```
exec --no-startup-id fcitx
```
を追加。

（これだけでOK）

## Google Chrome のインストール

snap 化によりいろいろ面倒くさくなってしまったので。

[【図解で理解】Ubuntu Desktop 20.04 LTSへのGoogle Chromeインストール手順 ](https://inab818.site/linux/ubuntu-20-04-lts-google-chrome-download-install/)


Google の配布サイトからダウンロードを選ぶと、dev パッケージが
ダウンロードできるので、それをファイルブラウザから
「ソフトウェアのインストール」を選んで実行するだけ。

※一度ダウンロードしたほうがよい。そのままインストールさせようとすると
失敗する。
