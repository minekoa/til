# TLA+ を apt でインストールする方法

Chromebook C202SAの crostini 環境だと、zipアーカイブ展開ではうまく行かなかったので。

## やりかた

以下の内容で `/etc/apt/sources.list.d/tlaplus.list` を作成する

```
deb https://tla.msr-inria.inria.fr/tlatoolbox/ci/toolboxUpdate/ ./
```

apt-key を足して ..

```
$ sudo wget -qO - tla.msr-inria.inria.fr/jenkins.pub | sudo apt-key add -
```

apt update

```
$ sudo apt update
```

あとは

```
$ sudo apt install tla+toolbox
```


## 参考

* [Install TLA+Toolbox on Ubuntu from APT repository on Vimeo](https://vimeo.com/193098941)
