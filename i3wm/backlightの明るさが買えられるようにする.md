# i3wm で バックライトの明るさを変える


## xbacklight をインストールする

```
sudo apt install xbacklight
```


## .comfig/i3.config に設定を追記

```
# Screen brightness controls
bindsym XF86MonBrightnessUp exec xbacklight -inc 10 # increase screen brightness
bindsym XF86MonBrightnessDown exec xbacklight -dec 10 # decrease screen brightness
```

## 参考

* [Ubuntu 14.04 で LCD の明るさをコマンドラインから変えたい | Qiita](https://qiita.com/xr0038/items/7a22a63f121ce2c205c7)

