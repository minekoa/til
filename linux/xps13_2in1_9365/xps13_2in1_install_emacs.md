# XPS 13 2in1 (9365) Ubuntu 16.04 LTS 環境構築 >> Emacs

## Emacs 26 のインストール

```sh
$ sudo apt install git bash-completion
$ sudp apt build-dep emacs24
```

`build-dep` を有効にするため `/etc/apt/sources.list` を開き、
有効な`deb http://hogehoge/..` については、その次の行で
コメントアウトされている `deb-src http://hogehoeg/...` を有効にします。

と宇宙でPosix Mail云々聞いてきますがが、「設定なす」を選択。

チェックアウトしてきてビルド。

```sh
$ mkdir src
$ cd src
$ git clone git://git.savannah.gnu.org/emacs.git
$ cd emacs
$ ./autoconf.sh
```

`make`, `make install` します

```sh
$ ./configure --prefix $HOME/usr/local --without-xim
$ make
$ make install
```

終えたら、

```sh
$ ~/usr/local/bin/emacs
```

で起動確認します。最後に、`~/.profile` に以下を追記してログイン➔ログアウト

```sh
if [ -d "$HOME/usr/local/bin" ] ; then
    PATH="$HOME/usr/local/bin:$PATH"
fi
```

## emacs.d

自分のリポジトリから持ってくる。

```
$ cd ~
$ git clone https://github.com/minekoa/.emacs.d.git
```

## Emacs の設定

### apt で入れるもの

```sh
$ sudo apt install fonts-vlgothic
$ sudo apt install
```

### emacs の M-x package-liste-package でいれたもの

* mozc
* color-theme
* elscreen
* migemo
