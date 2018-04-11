# XPS13 2in1 に Windows subsystem for Linux で環境を作る

## Ubuntu をインストールする


## GUI で動くようにする

* [Windows Subsystem for Linux + X Windowを1.024倍くらい使いこなすための方法 | Qiita](https://qiita.com/nishemon/items/bb3aca972404f68bfcd6)
* [http://ikesan009.sakura.ne.jp/2017/07/11/%E3%80%90%E6%96%B0%E3%80%91bash-on-ubuntu-on-windows%E3%81%A7windows%E4%B8%8A%E3%81%ABlinux%E7%92%B0%E5%A2%83%E3%82%92%E5%B0%8E%E5%85%A5%E3%81%99%E3%82%8B/

[VcXsrv公式サイト](https://sourceforge.net/projects/vcxsrv/?source=typ_redirect)にアクセスし、インストーラーをダウンロードしVcXsrvをインストールします。

ダウンロードが完了したら、VcXsrvのインストーラーを起動し、「Select the type of install」では「Full」を選択しインストールを完了させます。

エクスプローラのアドレス欄に `shell:startup` と入力し、開いたフォルダにVcXsrv のショートカットを放り込んで自動起動対象とする。

## Mozc の設定

emacs から使うために設定。

```
sudo apt install emacs-mozc
sudo apt install mozc-utils-gui
````

でインストール（依存関係にまかせる）

```
$ /usr/lib/mozc/mozc_tool --mode=config_dialog
```
で起動して、AZIC定義ファイルを読み込む

* [Mozc | archlinux](https://wiki.archlinux.jp/index.php/Mozc#.E3.82.B3.E3.83.9E.E3.83.B3.E3.83.89.E3.83.A9.E3.82.A4.E3.83.B3.E3.81.8B.E3.82.89_Mozc_.E3.83.84.E3.83.BC.E3.83.AB.E3.82.92.E8.B5.B7.E5.8B.95)

## Emacs をインストールする


```
sudo apt install emacs
```

自分のgithubから `.emacs.d` を取得する

apt で以下をいれる

```
sudo apt install font-vlgothic
sudu apt install migemo
sudo apt install gconf2
```

* [GConf-WARNINGが出てる時の対処法](http://highfrontier.ldblog.jp/archives/52259786.html)

`package-list-package`


あと、beep音消した。

```elisp
;; init.el

(setq ring-bell-fuction 'ignore)
```



sudo apt install x11-apps
