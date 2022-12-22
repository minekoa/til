# Lenovo ThinkCentre M710q Tiny に Ubuntu 22.4 LTS をインストール

## インストールディスクの作成

公式から `ubuntu-22.04.1-desktop-amd64.iso` をダウンロードしてきて
bootable USB Memory を作成。

作成には最初 [Universal USB Installer](https://www.softonic.jp/download/universal-usb-installer/) を使ったけど
bootに失敗して (BusyBox にいっちゃう）ので※、
[Rufus](https://rufus.ie/ja/#google_vignette) で作ったらうまく言った。

※Ubuntu16.04のインストールUSBを再利用したんだけど、Formatしてないみたいだったから
そのせいかも

## インストール

* キーボードレイアウト:  US
* サードパーティ製ソフトウェアのインストールする
    * セキュアブート設定を求められるようになった
* ディスクを削除してインストール


## 細々設定など

### ディレクトリ名を英語に

```console
LANG=C xdg-user-dirs-gtk-update
```

まずはこれ

### git を使えるようにする

til とか .emacs.d とか

```console
sudo apt install git
```

SSH-keyをつくって githubに登録

```console
ssh-keygen -t ed25519 -C "mineko.orange@gmail.com"
```

あとはブラウザからSSH-keyを登録する


### til

```console
cd reps
git clone git@github.ocm:minekoa/til.git
```

### emacs

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


### chrome

```console
cd ~/Download
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```
