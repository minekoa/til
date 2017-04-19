# XPS 13 2 in 1 (9365) に Ubuntu 16.04 LTS を入れる

* [DEL XPS13 (9350 2015/11モデル)にUbuntu15.10をデュアルブートインストール](http://qiita.com/NewGyu/items/d25cda12e0e1c5259121)
* [\[DELL XPS 13 9350\]Xubuntu 14.04 LTSをインストールした時の覚書](https://booleestreet.net/archives/10321)

## 回復ディスクの作成

しときましょう。(8GB以上のUSBメモリが必要です)

## Ubuntu 16.04 LTS 日本語 Remix のインストーラーの作成

[Ubuntu Desktop 日本語 Remixのダウンロード](https://www.ubuntulinux.jp/download/ja-remix)よりISOイメージをダウンロードします。

そしたら、`unetbootin` を使い、USBインストーラをこしらえます。
(ddでもよかったのだけれども、UEFI周りの面倒なことが起きないのを祈って Windows 向けツールで作成)

## BIOSセッティング

XPS 13 2 in 1 に Ubuntu16.04LTSを入れるには、

* SATA Operation の RAID 無効化
* セキュアブート無効化

が必要です。前者は、Ubuntu のインストーラからディスクが認識できません。

### BIOSの起動

* 起動メニューで F12 ➔ BIOS設定画面に入る

### SATA Operation の RAID 無効化

* 左メニューより[System Configration]を選ぶ ➔ System Configuration 画面
* System Configuration 画面で[SATA Operation]を選ぶ➔ SATA Operation 画面
* SATA Operation画面で選択肢を[RAID]から[AHCI]に変更

ACPIモードにすると、プリインストールのイメージは読めなくなります。
(つまり、Windows 10 が起動しなくなる)

今回はWindowsを消してしまうつもりなので問題ありませんが、デュアルブート環境を作る場合は、
回復ディスクから Windowsをインストールする必要があります。

### セキュアブートの無効化

* 左メニューより [Secure Boot] を選ぶ ➔ Secure Boot 画面 で Secure Boot を選ぶ
* 項目 [Enable Secure Boot] のチェックを外す

## Ubuntu 16.04 LTS のインストール

普通にできるはずです。

## ネットワークがつながらない

インストールは無事おえたのですが、ネットワークにつながりません。`ifconfig` してもワイヤレスアダプターが見えないので、ドライバーの問題。

ググってみれば、以下のスレッドが。

* [XPS 9365 2-in-1, Ubuntu? : linuxhardware](https://www.reddit.com/r/linuxhardware/comments/5nckec/xps_9365_2in1_ubuntu/)

> I have that machine and I installed Ubuntu 16.04 on it. Most things work after you install 4.10 from kernel-ppa and add firmware for wifi and gpu from intel. Two things that do not work:
> 
> * normal resume from the sleep (S3). It kind of works but requires holding the power button for over 5s which is really troublesome given that on Windows it wakes up instantly and the power button is very inconvenient to press (small, hard to press)
> * fingerprint reader doesn't work

...了解です。

### カーネルのインストール

カーネルのインストールは、

[How to Install Kernel 4.10 in Ubuntu / Linux Mint](http://ubuntuhandbook.org/index.php/2017/02/install-kernel-4-10-ubuntu-linux-mint/)

の

> ```
> cd /tmp/
>  
> wget http://kernel.ubuntu.com/~kernel-ppa/mainline/v4.10/linux-headers-4.10.0-041000_4.10.0-041000.201702191831_all.deb
>  
> wget http://kernel.ubuntu.com/~kernel-ppa/mainline/v4.10/linux-headers-4.10.0-041000-generic_4.10.0-041000.201702191831_amd64.deb
>  
> wget http://kernel.ubuntu.com/~kernel-ppa/mainline/v4.10/linux-image-4.10.0-041000-generic_4.10.0-041000.201702191831_amd64.deb
>  
> sudo dpkg -i *.deb
> ```

を参考に。`.deb`をUSBメモリーに入れて、XPS 13 の Ubuntu 環境上で `dpkg` すればよいです。

最後に、カーネルが更新されたことを`uname -r` で確認

```sh
$ uname -r
4.10.0-041000-generic
```

### ワイヤレスアダプター用ファームウェアのインストール

[Dellのサポートページ](http://www.dell.com/support/home/us/en/19/product-support/product/xps-13-9365-2-in-1-laptop/driver?newtab=true)を見ると、`Intel 8265 WIFI Driver` であるみたいです。

[インテル®ワイヤレス・アダプター用の Linux* サポート ](http://www.intel.co.jp/content/www/jp/ja/support/network-and-i-o/wireless-networking/000005511.html) から
[インテル® Dual Band Wireless-AC8265 | 8265-ucode-22.361476.0iwlwifi-.tgz](https://wireless.wiki.kernel.org/_media/en/users/drivers/iwlwifi-8265-ucode-22.361476.0.tgz) をダウンロード.
展開して得られる `iwlwifi-8265-22.ucode` を `/lib/firmware` にコピーして再起動すれば繋がりました。


## いつもの設定

### ホーム直下のディレクトリを英語にする

```sh
$ LANG=C xdg-user-dirs-gtk-update
```

## 未解決問題

サスペンドから復帰できません。
