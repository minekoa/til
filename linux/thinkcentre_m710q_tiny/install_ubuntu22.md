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
ssh-keygen -t ed25519 -C "xxxxxx@example.com"
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

### AZIK MOZK

ローマ字テーブルはこちらから取得
https://gist.github.com/youcune/8149126

```console
$ /usr/lib/mozc/mozc_tool --mode=config_dialog
```

で設定ダイアログを開き、[キー設定]→[ローマ字テーブル]→[編集..] から
ローマ字テーブルのインポートを行う。

ついでに、上のテーブルだと `{'-', 'ー'}` がいないので
追加しておく。


### ネットワークの調子が悪い

長時間稼働してると？DNSが腐る問題。

```
DNS_PROBE_STARTED
```

とりあえず、DNSサーバーをGoogleのものに変更してみて様子見

参考: [Ubuntu 20.04 LTSでDNSの設定をする。](https://freefielder.jp/blog/2021/06/ubuntu-20-04-dns-settings.html)

```console
$ cat /var/run/systemd/resolve/resolv.conf
# This is /run/systemd/resolve/resolv.conf managed by man:systemd-resolved(8).
# Do not edit.
#
# This file might be symlinked as /etc/resolv.conf. If you're looking at
# /etc/resolv.conf and seeing this text, you have followed the symlink.
#
# This is a dynamic resolv.conf file for connecting local clients directly to
# all known uplink DNS servers. This file lists all configured search domains.
#
# Third party programs should typically not access this file directly, but only
# through the symlink at /etc/resolv.conf. To manage man:resolv.conf(5) in a
# different way, replace this symlink by a static file or a different symlink.
#
# See man:systemd-resolved.service(8) for details about the supported modes of
# operation for /etc/resolv.conf.

nameserver 8.8.8.8
nameserver 8.8.4.4
search .
```

と変えたものの、状況変わらず。
Networkの再接続で治るっぽい。
（アクセスできるところとできないところがある）



## Erlang

* [Erlang 25.2 をインストールする](../../erlang/install-erlang-25.2-in-ubuntu-22.4.md)
* [rebar3 のインストール](../../erlang/install-rebar3.md)


### パスの追加

手前ビルド用にパス `~/usr/local/bin` 同 `/sbin` と rebar3 の install local したパス `~/.cache/rebar3/bin` を追加。

`.bashrc` に以下を追記して

```
export PATH=$PATH:$HOME/usr/local/bin::$HOME/usr/local/sbin
```


```console
source .bashrc
```

## nodejs

PPAを追加してインストール

```console
$ curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -

## Installing the NodeSource Node.js 18.x repo...


## Populating apt-get cache...

+ apt-get update
ヒット:1 https://dl.google.com/linux/chrome/deb stable InRelease
ヒット:2 http://security.ubuntu.com/ubuntu jammy-security InRelease
ヒット:3 http://jp.archive.ubuntu.com/ubuntu jammy InRelease
ヒット:4 http://jp.archive.ubuntu.com/ubuntu jammy-updates InRelease
ヒット:5 http://jp.archive.ubuntu.com/ubuntu jammy-backports InRelease
パッケージリストを読み込んでいます... 完了

## Confirming "jammy" is supported...

+ curl -sLf -o /dev/null 'https://deb.nodesource.com/node_18.x/dists/jammy/Release'

## Adding the NodeSource signing key to your keyring...

+ curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | gpg --dearmor | tee /usr/share/keyrings/nodesource.gpg >/dev/null

## Creating apt sources list file for the NodeSource Node.js 18.x repo...

+ echo 'deb [signed-by=/usr/share/keyrings/nodesource.gpg] https://deb.nodesource.com/node_18.x jammy main' > /etc/apt/sources.list.d/nodesource.list
+ echo 'deb-src [signed-by=/usr/share/keyrings/nodesource.gpg] https://deb.nodesource.com/node_18.x jammy main' >> /etc/apt/sources.list.d/nodesource.list

## Running `apt-get update` for you...

+ apt-get update
取得:1 https://deb.nodesource.com/node_18.x jammy InRelease [4,563 B]
ヒット:2 https://dl.google.com/linux/chrome/deb stable InRelease
ヒット:3 http://jp.archive.ubuntu.com/ubuntu jammy InRelease
ヒット:4 http://security.ubuntu.com/ubuntu jammy-security InRelease
ヒット:5 http://jp.archive.ubuntu.com/ubuntu jammy-updates InRelease
取得:6 https://deb.nodesource.com/node_18.x jammy/main amd64 Packages [774 B]
ヒット:7 http://jp.archive.ubuntu.com/ubuntu jammy-backports InRelease
5,337 B を 1秒 で取得しました (4,476 B/s)
パッケージリストを読み込んでいます... 完了

## Run `sudo apt-get install -y nodejs` to install Node.js 18.x and npm
## You may also need development tools to build native addons:
     sudo apt-get install gcc g++ make
## To install the Yarn package manager, run:
     curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | gpg --dearmor | sudo tee /usr/share/keyrings/yarnkey.gpg >/dev/null
     echo "deb [signed-by=/usr/share/keyrings/yarnkey.gpg] https://dl.yarnpkg.com/debian stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
     sudo apt-get update && sudo apt-get install yarn

```
```console
$ sudo apt show nodejs
Package: nodejs
Version: 18.13.0-deb-1nodesource1
Priority: optional
Section: web
Maintainer: Ivan Iguaran <ivan@nodesource.com>
Installed-Size: 184 MB
Provides: nodejs-dev, nodejs-doc, nodejs-legacy, npm
Depends: libc6 (>= 2.17), libgcc1 (>= 1:3.4), libstdc++6 (>= 5.2), python3-minimal, ca-certificates
Conflicts: nodejs-dev, nodejs-doc, nodejs-legacy, npm
Replaces: nodejs-dev (<= 0.8.22), nodejs-legacy, npm (<= 1.2.14)
Homepage: https://nodejs.org
Download-Size: 28.4 MB
APT-Sources: https://deb.nodesource.com/node_18.x jammy/main amd64 Packages
Description: Node.js event-based server-side javascript engine
 Node.js is similar in design to and influenced by systems like
 Ruby's Event Machine or Python's Twisted.
 .
 It takes the event model a bit further - it presents the event
 loop as a language construct instead of as a library.
 .
 Node.js is bundled with several useful libraries to handle server tasks :
 System, Events, Standard I/O, Modules, Timers, Child Processes, POSIX,
 HTTP, Multipart Parsing, TCP, DNS, Assert, Path, URL, Query Strings.

N: 追加レコードが 1 件あります。表示するには '-a' スイッチを付けてください。
```

```console
$ sudo apt install nodejs
パッケージリストを読み込んでいます... 完了
依存関係ツリーを作成しています... 完了
状態情報を読み取っています... 完了
以下のパッケージが自動でインストールされましたが、もう必要とされていません:
  chromium-codecs-ffmpeg-extra gstreamer1.0-vaapi i965-va-driver intel-media-va-driver libaacs0
  libaom3 libass9 libavcodec58 libavformat58 libavutil56 libbdplus0 libblas3 libbluray2 libbs2b0
  libchromaprint1 libcodec2-1.0 libdav1d5 libflashrom1 libflite1 libftdi1-2 libgme0 libgsm1
  libgstreamer-plugins-bad1.0-0 libigdgmm12 liblilv-0-0 libmfx1 libmysofa1 libnorm1 libopenmpt0
  libpgm-5.3-0 libpostproc55 librabbitmq4 librubberband2 libserd-0-0 libshine3 libsnappy1v5
  libsord-0-0 libsratom-0-0 libsrt1.4-gnutls libssh-gcrypt-4 libswresample3 libswscale5
  libudfread0 libva-drm2 libva-wayland2 libva-x11-2 libva2 libvdpau1 libvidstab1.1 libx265-199
  libxvidcore4 libzimg2 libzmq5 libzvbi-common libzvbi0 mesa-va-drivers mesa-vdpau-drivers
  pocketsphinx-en-us va-driver-all vdpau-driver-all
これを削除するには 'sudo apt autoremove' を利用してください。
以下のパッケージが新たにインストールされます:
  nodejs
アップグレード: 0 個、新規インストール: 1 個、削除: 0 個、保留: 5 個。
28.4 MB のアーカイブを取得する必要があります。
この操作後に追加で 184 MB のディスク容量が消費されます。
取得:1 https://deb.nodesource.com/node_18.x jammy/main amd64 nodejs amd64 18.13.0-deb-1nodesource1 [28.4 MB]
28.4 MB を 3秒 で取得しました (10.8 MB/s)
以前に未選択のパッケージ nodejs を選択しています。
(データベースを読み込んでいます ... 現在 216517 個のファイルとディレクトリがインストールされています
。)
.../nodejs_18.13.0-deb-1nodesource1_amd64.deb を展開する準備をしています ...
nodejs (18.13.0-deb-1nodesource1) を展開しています...
nodejs (18.13.0-deb-1nodesource1) を設定しています ...
man-db (2.10.2-1) のトリガを処理しています ...

$ node --version
v18.13.0
```
