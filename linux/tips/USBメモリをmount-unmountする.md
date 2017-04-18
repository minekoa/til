# USBメモリをmount/unmountする.md

別段大した話ではないが、せっかく書いたのでコード片をば。

## mount

```bash
#!/bin/bash
set -eu

dev=$(sudo parted -l -m | egrep "SanDisk Extreme" | cut -d : -f 1) # よく使うUSBメモリ名を使うといいよ

echo $dev
if [ -z $dev ] ; then
    exit 1
fi

mdir="/media/usbmem"

if [ -e $mdir ]; then
   echo "$mdir is already exists."
else
   sudo mkdir $mdir
   echo "create $mdir"
fi
sudo mount -t vfat "${dev}1" ${mdir}

mount | grep -i "${dev}1"
```

## unmount

```
#!/bin/bash
set -eu

mdir="/media/usbmem"
if [ -e $mdir ]; then
   sudo umount ${mdir}
fi
```

