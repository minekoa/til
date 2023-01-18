# wine でセシル変身アプリを動かす

## wine のインストール

https://wiki.winehq.org/Ubuntu


リポジトリキーのダウンロード

```console
sudo mkdir -pm755 /etc/apt/keyrings
sudo wget -O /etc/apt/keyrings/winehq-archive.key https://dl.winehq.org/wine-builds/winehq.key
```

PPA追加

``` console
sudo wget -NP /etc/apt/sources.list.d/ https://dl.winehq.org/wine-builds/ubuntu/dists/jammy/winehq-jammy.sources
```

インストール

``` console
sudo apt update
sudo apt install --install-recommends winehq-staging
```

stable , development, staging がある。stableにパッチあてたのが stagingらしいのでこちらを
インストール


## vulkan

[【第16回】おっとIntelのdGPUはUbuntu用のドライバがリリースされているだと…… よっしゃNVIDIAもAMDもまとめてDeskMeet X300で動作させるぞ！(Intelと簡易ベンチマーク編)](https://pc.watch.impress.co.jp/docs/column/ubuntu/1460555.html)

```console
$ wget -qO - https://repositories.intel.com/graphics/intel-graphics.key | sudo gpg --dearmor --output /usr/share/keyrings/intel-graphics.gpg
$ echo 'deb [arch=amd64,i386 signed-by=/usr/share/keyrings/intel-graphics.gpg] https://repositories.intel.com/graphics/ubuntu jammy arc' | sudo tee  /etc/apt/sources.list.d/intel.gpu.jammy.list
$ sudo apt update && sudo apt upgrade
```

```console
$ sudo apt install linux-oem-22.04
```

これは余計だったと思う（けど思考停止でやっちゃった）。

本当に必要だったのはこちら


```
$sudo apt install libvulkan1:i386
$sudo apt install libgl1:i386
```

## 環境作成

[セシル変身アプリを wine で動かす](https://the.kalaclista.com/notes/%E3%82%BB%E3%82%B7%E3%83%AB%E5%A4%89%E8%BA%AB%E3%82%A2%E3%83%97%E3%83%AA%E3%82%92-wine-%E3%81%A7%E5%8B%95%E3%81%8B%E3%81%99/)

dxvk使う方

```
$ env WINEPREFIX=~/wine/cecil wineboot
$ env WINEPREFIX=~/wine/cecil wineboot winetricks corefonts dxvk
```

dxvk使わないほう

```
$ env WINEPREFIX=~/wine/cecil2 wineboot
$ env WINEPREFIX=~/wine/cecil2 wineboot winetricks corefonts
```

どちらも `drive-c` の下に セシル変身をコピーして利用。こんな感じ

```
$ cd ~/wine/cecil/drive_c/CecilHenShin
$ env WINEPREFIX=~/wine/cecil wine CecilHensin.exe
```

みたいな感じ

## まとめ

* dxvk 使う方は動かなかった。セシル側の問題じゃなくっておそらく環境側
* dxvk 使わない、古いセシル変身だけ起動した
* ItemShop は動かなかった

どの場合でも、起動オプション聞く画面までは動いた。
