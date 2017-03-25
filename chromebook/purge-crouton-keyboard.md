# crouton の keyboard の無効化

追加インストールは `-u` オプションで出来るのですが、
残念ながら、`crouton` にはアンインストールオプションはないので、crouton 環境の中に入って、

```
sudo apt-mark unhold xkb-data
sudo apt-get -y install --reinstall xkb-data
```

します。

## 参考

* [Keyboard - dnschneid/crouton wiki - GitHub](https://github.com/dnschneid/crouton/wiki/Keyboard)

