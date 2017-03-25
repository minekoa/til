# Chromebook Flip C302CA のLinux環境構築

## デベロッパーモード

`ESC` + リロード + 電源ボタン で chromebook を起動します。

レスキューモードが立ち上がるので、`Ctrl-D`  を押してデベロッパーモードにします。

（すべての設定が消えるのでご注意）

## crouton (ubuntu xenial)

`Ctrl-Alt-T` を押下するとブラウザタブにchrosh が起動します。ここで

```
$ shell
```

とタイプして、bash を起動します（これを行うためにデベロッパーモードが必要でした）。

crouton を取得します。

```
$ wget http://goo.gl/fd3zc -O ~/Download/crouton
```

早速croutonを実行します

```
$ sudo sh -e ~/Downloads/crouton -r xenial -t xfce,cliextra,extension,xiwi
```

`-r` はインストールしたいディストリビューションを(今回 ubuntu 16.04LTS を指定しました)指定します。ただし、ubuntu は trusty(14.04LTS) 以外はサポートなしなので、debianとかにしたほうが良いかもしれません。（実際、日本語環境化は難航しました）

`-t` はパッケージを指定します。今回指定したパッケージは以下のとおりです。

|オプション|意味|
|--------------|------|
|xfce|xfceデスクトップがインストールされます。ただし、xfceのアプリケーション類はインストールされません(必要な場合は `xfce-desktop` を指定します)。インストール後、`sudo startxfce4` で xfceデスクトップが起動します|
|cli-extra|基本的なCLIツールです。`sudo enter-chroot` でCLIで環境が起動します。|
|xiwi|ChromeOS の windows としてデスクトップを開けます. これを使用する場合、ChromeOS 側に chrome拡張「crouton integration」 を追加する必要があります。|
|extension|上記環境の場合のクリップボード共有とか。実はしていしなくても xiwi だけ指定すればよいはず...(^^;|
|keyboard|ChromeOSの「画面の明るさ」などのキー(ファンクションキーの部分にあるもの )を操作可能にします。Searchキーを押しながら操作します。（そのかわり Search キーは `Super_L` から `Overlay1_Enable`になります|

詳しくは `sudo sh ~/Downloads/crouton -t help` を見てください。

結構時間がかかるので、気長に待ちます。最後にユーザー名とパスワードが聞かれるので入力しておしまいです。

## crouton integration のインストールと起動

まず、ChromeOS側で、[crouton integration - Chrome Webストア](https://chrome.google.com/webstore/detail/crouton-integration/gcpneefbbnfalgjniomfjknbcgkbijom) から crouton integration をインストールします。

その後、crosh -> shell から、
```
$ sudo startxfce4
```
を実行すると、xfce デスクトップが開きます。

最初はフルスクリーンなので、ChromeOS 側にアクセスしたいときは 最大化ボタン、最小化ボタンを一度押せばOKです。


既知の問題
:  crouton integration 中で Ctrl-Space を受けるとき、Spaceキーがリピートしっぱなしになるようです。既存のChromebook Flip C100PA の環境(ChromeOS 54.0) では発生せず、この Chromebook Flip C302CA (同56.0) にてのみ発生しています。Web上の悲鳴をみるに、どうやら ChromeOS 55.0 (2017-01-10 stable リリース) から発生したようです。
: この問題を回避するには、(1) ChromeOS 側の入力方式を一つにする（つまりGoogle日本語入力を選択肢リストから外す）(2) crouton integration を使わない が考えられます。
: どちらにしても、ChromeOS とのシームレスなやりとりと、それを期待したLinux側のディスク容量の節約は難しくなります。

### crouton integration を使わない起動

少し寄り道。先程の既知の問題を回避する方法の一つ、crouton integration を使わない場合の説明です。

crouton integration を使わない場合、crouton イメージには xorg パッケージが必要です。

あとから追加する場合は以下のようにします。

 ```
 $ sudo sh -e ~/Downloads/crouton -r xenial -u -t xorg
 ```
 
こうしておいて、 `sudo startxfce4 -X xorg` や `sudo startxfce4 -X xiwi` のようにすることで、どちらで起動するかを選ぶことができます。

#### crouton integration を使わない場合のデスクトップ切り替え
 `Ctrl+Shift+Alt+←(F1)` または `Ctrl+Shift+Alt+→(F2)`で切り替わります。

この時注意するのは、ChromeOS や crouton integration 側は、ChromeOSの設定で入れ替えたあとのキーではなく、キーボードに刻印されているもともとのキーであることです。たとえば、Ctrl と Search を入れ替えている場合でも、Ctrlは右下あるいは左下のキーになります。

一方で、Linux Desktop の場合、`xmodmap` などでキーを入れ替えていた場合は、入れ替えたあと(つまり`keysym`で Ctrl や Alt や Shift になっているもの)のキーで入力します。


## 日本語入力環境

crouton trusty  のときは、普通の Ubuntu の日本語化をすれば問題なくできたのですが、xenial はサポート外というだけあって、結構ハマりました。

ここではうまく行った手順を示します。



## Emacs 26 

まずはビルド環境をつくります
```
$ sudo apt install git
$ sudo apt build-dep emacs24
```

途中で Posix Mail云々きかれるが、「設定なし」を選択。

```
$ mkdir src
$ cd src
$ git clone git://git.savannah.gnu.org/emacs.git
$ cd emacs
$ ./autoconf.sh
```

make
```
$ ./configure --prefix $HOME/usr/local --without-xim
$ make
$ make install
```

インストールが終わったら、

```
~/usr/local/bin/emacs
```

して、無事起動することを確認してください。

最後に ~/.profile  に以下を追記して、パスを通します。

```
export PATH=$HOME/usr/local/bin:$PATH
```

一度ログアウトして、ログインしなおして emacs がつかえればOKです。

###  ごく個人的なemacsの設定

秘伝の emacs.d を持ってきます。

そこで必要とされる環境をインストール。まずは、

```
$ sudo apt install fonts-vlgothic
$ sudo apt install emacs-mozc-bin
$ sudo apt install cmigemo
```

その後Emacs上で
```
M-x list-package
```

で表示されるパッケージの中から 

* mozc 
* color-theme 
* elscreen
* migemo

をインストールします。

## i3wm のインストール

最近どっぷりとお気に入りのタイル型ウィンドウマネージャ i3 (i3wm) を使えるようにしてみましょう。

実は普通にxfce をいれると i3自体はインストールされています。なのであとはそちらで起動するだけ、と思いきや、dmenu や i3status が入っていないため、エラーになってしまいます(なんとも中途半端な)。

しかたがないので個別にいれます。

```
sudo apt install dmenu
sudo apt install i3status
```

この状態で簡単にi3を起動できるようにするため、ChromeOS側で、

```
#!/bin/sh -e
set -e
APPLICATION="${0##*/}"

USAGE="$APPLICATION [options]

Wraps enter-chroot to start a i3 session.
By default, it will log into the primary user on the first chroot found.

Options are directly passed to enter-chroot; run enter-chroot to list them."

exec sh -e "/usr/local/bin/enter-chroot" "$@" exec xinit /usr/bin/i3
```

を `/usr/local/bin/starti3` として置きます（これは `startxfce4` のパクリです)。

置き方はたとえばこんな感じ。普通に caret などのテキストエディタで上記ファイルを Downloads フォルダにつくってから、
```
$ sudo cp ~/Downloads/starti3 /usr/local/bin/starti3
$ sudo chmod 755 /usr/local/bin/starti3
```

### keymapの変更

もともと、ChromeOS 側でおこなったキーリマップは xorg オプションで起動したデスクトップ環境には引き継がれません。

さらに、Ctrl-Space のリピート問題に効くのでは、とうっかり crouton の keyboard オプションをいれたら、Search ボタンが `Super_L` ではなく `Overlay1_Enable` になってしまいました。これは本当に困ります。

keymap の変更が必要です。

（ついでに crouton-integraton つかいたいカジュアルなときは xfce, そうでないときは i3という使い分けをしたいという気持ちに答えるセッティングをしてみます）

|物理キー(keycode)|キーシンボル|
|--------------|------------|
|Searchキー(133)| 左Ctrl   |
|左Ctrl (37)| 左Super|
|右Ctrl (105) | オーバーレイ1有効|

のようなキー配置にしたいです（オーバーレイは  Linux 側から画面の明るさやボリュームを変えたいので使わない右Ctrl に割り当てました)。

~/.config/i3/i3_xmodmap
```
clear control
clear mod4

keycode 133 = Control_L
keycode 37 = Super_L
keycode 105 = Overlay1_Enable

add control = Control_L Control_R
add mod4 = Super_L
```



~/.config/i3/config

```
exec xmodmap ~/.config/i3/i3_xmodmap

# i3 config file (v4)
#
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4
```

Search キーのキーコードが `133`,  Control(左) キーのキーコードが `37` です。

これで、

```
$ sudo starti3 -X xorg
```

で起動すると幸せになれるはず。`-X xiwi` で起動するとchromebook のキースワップ設定が作ったキーコードが送られるため、混乱の局地にいざなわれますが...。

note
: fcitx を起動すると xmodmap が勝手に上書きされます。.Xmodmap に設定を記述すれば、fcitx が上書きするときにそれを読んでくれるそうですが、i3wm でだけ設定を効かせたいとかやるの面倒ですね。
: この問題に対処するのは課題です。

### 壁紙

i3wm自体に壁紙を設定する機能はないので、壁紙の変更は feh コマンドで行います。

```
sudo apt install feh
```

`~/.config/i3/config` に以下を追記します

```
exec --no-startup-id feh --bg-scale $HOME/.config/i3/i3wm.png
```

ところでi3wm っぽい壁紙ですが、[reddit の i3wm subreddit](https://www.reddit.com/r/i3wm/)の[あれ](https://www.reddit.com/r/i3wm/comments/3hqkst/solarized_i3_wallpaper_1080p/) とか [これ](https://www.reddit.com/r/i3wm/comments/3es55n/just_a_minimalis_wallpaper_i_made_enjoy/) に良い壁紙が転がっていました。

