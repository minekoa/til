# crouton xenial 環境に Elm をインストールする

## 最新の Node.js のインストール

n パッケージをつかってインストールしたいのですが、そのために一度 apt で npm をインストールします。


```
$ sudo apt install npm
```

`npm -g` のインストール先を変更します (これは将来のメモリ逼迫時に `$HOME/usr/local` を SDメモリーカードに逃がすなど出来るといいな、的なケチケチ作戦の布石です)

```
$ npm config set prefix $HOME/usr/local
```


で、`n` パッケージをインストール

```
$ npm install n -g
```

ところがこれが大失敗。n コマンドでのインストール先が変更できないみたい...orz

で、さっきの変更は 当然 rootにとってはパスが通っていないので、`sudo` との相性は最悪です。

```
$ sudo /home/hoge/usr/local/bin/n stable
```

どうしてこうなった...orz

```
$ node -v
v7.6.0
```

なにはともあれ、インストールできたので、ブートストラップ訳はお役ごめんです。

```
$ sudo apt purge npm nodejs
$ sudo apt autoremove
```

なんか　npm だけ 使おうとすると

```
bash: /usr/bin/npm: そのようなファイルやディレクトリはありません
```

と怒られるので、すごくいい加減だけれども、

```
$ sudo ln -sf /usr/local/bin/npm /usr/bin/npm
```

しました。

### Elmのインストール

ここまで出来たら簡単です。

```
$ npm install -g elm
```

インストールできたか確認。

```
$ elm -v
0.18.0
```

## 参考

* [Ubuntuに最新のNode.jsを難なくインストールする | Qiita](http://qiita.com/seibe/items/36cef7df85fe2cefa3ea)
* [グローバルにインストールしたnode moduleがnot foundになる時の対処 | Qiita](http://qiita.com/joe-re/items/12987cdeee506dea3889)

