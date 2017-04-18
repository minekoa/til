# ディスクイメージのマウント(ddで吸いだしたやつ)

まず fdisk で .img の内容を確認する。ほしい情報は

* セクタサイズ
* 開始位置

```
$ sudo fdisk -u -l file_server/disk_images/usbmem.img 
Disk file_server/disk_images/usbmem.img: 29.2 GiB, 31376707072 bytes, 61282631 sectors
Units: sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disklabel type: dos
Disk identifier: 0x000ad85a

デバイス                                          起動    Start 最後から セクタ  Size Id タイプ
file_server/disk_images/usbmem.img1 *    57075712 61282303  4206592    2G 82 Linux スワ
file_server/disk_images/usbmem.img2          2046 57075711 57073666 27.2G  5 拡張領域
file_server/disk_images/usbmem.img5          2048 57075711 57073664 27.2G 83 Linux

Partition table entries are not in disk order.
```

開始位置×セクタサイズ がマウントのオフセットになる。
ファイルシステムを指定してマウントしてみる。
（間違っていたらマウントできないだけなので、トライアンドエラーで良い）

```
$ sudo mount -o loop,offset=$((512*2048)) file_server/disk_images/usbmem.img /media/keijuterazono/tmp/
```

### 参考

* [ddコマンドで保存したイメージファイルをマウント](http://opendream.co.jp/index.php/ddコマンドで保存したイメージファイルをマウント/)
* [ddで.imgファイルをマウントする方法](www5.atpages.jp/syockit/linux/mountdding.html)
