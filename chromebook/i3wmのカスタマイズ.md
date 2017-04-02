# i3wm のカスタマイズ

[i3 wm タイル型ウィンドウマネージャを使う|Jenemal Notes](http://malkalech.com/i3_window_manager) をみて、もうすこしカスタマイズしてみようかと思いました。

### ステータスバーの設定

 ```
$ sudo apt install compton
$ sudo apt install ttf-dejavu
```

### ステータスバーの設定

```
bar {
        status_command i3status
#        tray_output primary
        font pango:DejaVu Sans 11px
}
```

`tray_output primary` をなくすと、トレイにアイコンが出るよになる。

結果を確認するには `Shift+$mod+r` でOK.

* [i3 - ArchWiki](https://wiki.archlinuxjp.org/index.php/I3#i3status)


## テーマを設定する


Qt に GTK+ の設定を適用するには、環境変数に

```
QT_STYLE_OVERIDE=GTK+
```

を設定するのか、はたまた

```
~/.config/Trolltech.conf
...
[Qt]
style=GTK+
...
```

* [[SOLVED]i3wm - how to theme qt applications / Applications & Desktop Environments Arch Linux Forums](https://bbs.archlinux.org/viewtopic.php?id=213445)


### 手のひら検出機能を有効にする

xiwi では何も考えずに快適設定で動くのですが、`ctrl-space` 問題のせいで、emacsian にとっては、xiwiは、使い物にならなくなりました。

で、xorg で起動するとイライラさせる要因の一つが、キーボード入力中に、タッチパッドが反応してしまうこと。


* [Arch LinuxのTouchPadを快適にする設定](https://syui.github.io/blog/2014/07/24/archlinux-macbook-touchpad/)

を参考に手のひら検出をONにしてみます。

```
$ synclient -l | grep PalmDetect
    PalmDetect              = 0
$ synclient PalmDetect=1
```

うーん、効いているのかなぁ。あんまり聞いていないように思えます。

ChromeOS側にいるときは、タッチのし始めはタッチパッドのハシの方は反応していないようですので、そういう設定にできないかなぁ。

*[タッチパッドに手のひらが触れてカーソルが動くのを防止する | Qiita](http://qiita.com/uchan_nos/items/ccc4ef7e319cb6200cc9)


