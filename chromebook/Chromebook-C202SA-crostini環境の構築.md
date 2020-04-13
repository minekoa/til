# Chromebook C202SA の環境構築

http://wazalabo.com/crostini-japanese.html

を参考に日本語化

```
$ echo $LANG
en_US.UTF-8# ロケール一覧 -> ja_JP.UTF-8 が無い
$ locale -a
C
C.UTF-8
en_US.utf8
POSIX# ロケール作成
$ sudo dpkg-reconfigure locales
-> ja_JP.UTF-8 を生成
# 作成できたことの確認
$ locale -a | grep ja_JP
ja_JP.utf8

# ロケールの設定と設定の永続化
export LANG=ja_JP.UTF-8
echo ‘export LANG=ja_JP.UTF-8′ >> ~/.bashrc
```

.bashrc に以下の二行を追加

```
export LANG=ja_JP.UTF-8
alias ll='ls -al --color=auto'
```

## npm インストール

Elm を入れたいので npm を入れてみた

[Debian9 (stretch) に Node.js をインストールする | Qiita](https://qiita.com/naoyukiyama/items/29054cff00923f9543ce)

```
$ curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
$ sudo apt-get install -y nodejs
```

```
zoniathena@penguin:~$ nodejs -v
v10.16.0
zoniathena@penguin:~$ npm -v
6.9.0
```

で、Elmのインストール

```
$ sudo npm install -g elm
/usr/bin/elm -> /usr/lib/node_modules/elm/bin/elm

> elm@0.19.0-no-deps install /usr/lib/node_modules/elm
> node install.js

-- ERROR -----------------------------------------------------------------------

I had some trouble writing file to disk. It is saying:

Error: EACCES: permission denied, open '/usr/lib/node_modules/elm/bin/elm'

NOTE: You can avoid npm entirely by downloading directly from:
https://github.com/elm/compiler/releases/download/0.19.0/binary-for-linux-64-bit.gz
All this package does is download that file and put it somewhere.

--------------------------------------------------------------------------------

npm ERR! code ELIFECYCLE
npm ERR! errno 1
npm ERR! elm@0.19.0-no-deps install: `node install.js`
npm ERR! Exit status 1
npm ERR!
npm ERR! Failed at the elm@0.19.0-no-deps install script.
npm ERR! This is probably not a problem with npm. There is likely additional logging output above.

npm ERR! A complete log of this run can be found in:
```


```
$ npm config get prefix
/usr
```

おぉう。なんかここが /usr だと都合が悪いそうな。で
ホーム以下にnpm用のディレクトリを作ってあげる

```
$ mkdir ~/.npm-global
zoniathena@penguin:~$ npm config set prefix '~/.npm-global'
zoniathena@penguin:~$
zoniathena@penguin:~$ export PATH=~/.npm-global/bin:$PAT
```

永続化のため.profile に以下を追加した

```
if [ -d "$HOME/.npm-global/bin" ] ; then
    PATH="$HOME/.npm-global/bin:$PATH"
fi
```


参考 [npmでパッケージをインストールする](https://www.storange.jp/2017/02/npm.html)
