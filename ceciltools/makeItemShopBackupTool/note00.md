# ItemShopバックアップツールを作る no.1

## 基本的な情報のありか

ということでWindows上ではレジストリに保存されている。

```
\HKEY_CURRENT_USER\DreamFactory\ItemShop
```

## キー名称の法則

とりあえず、アクティブなスロット（ロードしているスロット）を特定する。

ItemShop はアイテムスロットが20個ある。


```
HogeHoge0_h..........
```

`HogeHoge`はそれっぽい名前、その後の数字が 0 オリジンのItemBox番号っぽい。
`_h.........` は不明。UnityがつけるオブジェクトIDかしら？
ここがインストールした環境ごとに異なるのかは不明。


試しに

`ItemSizeXSlider0_h125436664` というレジストリキー（REG_DWORD)をみたら
`00 00 00 00 A4 70 E7 BF` が格納されていた。

ItemShop上で見ると `-0.7325` が格納されていた。

```python
import struct
print( 'd: %s' % struct.unpack('d', bytes.fromhex('00000000A470E7BF'))[0])
```

```
d: -0.7325005531311035
```

なのでこれであっているっぽい。


## アイテムの基本要素を調べる


| 設定項目              | レジストリキー名 ( `n`: Box番号 )      |
|-----------------------|----------------------------------------|
| 選択されたItem        | `ItemBoxButton{n}_h...`                |
| テクスチャパターン(1) | `ItemXTexSlider{n}_h...\               |
| 位置                  | `ItemMove{X/Y/Z}Slider{n}_h...`        |
| 姿勢                  | `ItemRot{X/Y/Z}Slider{n}_h...`         |
| 大きさ                | `ItemSize{X/Y/Z}Slider{n}_h...`        |
| 装着位置              | `ItemBoneSlider{n}_h..`                |
| 色(1)                 | `ItemColoir{R/G/B}Slider{n}_h...`      |
| テクスチャパターン(2) | `ItemTexSlider(1){n}_h...`             |
| 色(2)                 | `ItemColoir{R/G/B/A}Slider(1){n}_h...` |
| シェイプ(1)           | `ItemShapeSlider{n}_h...`              |
| シェイプ(2)           | `ItemShapeSlider(1){n}_h...`           |
| シェイプ(3)           | `ItemShapeSlider(2){n}_h...`           |
| シェイプ(4)           | `ItemShapeSlider(3){n}_h...`           |




## 予想

```
ItemMoveXSlider{n:0..20}_h..
ItemMoveXSlider{n:0..255?}Item_h..
ItemMoveXSlider{n:0..255?}Item{m:0..20}_h..
ItemMoveXSlider{n:1..12}Preset{m:0..20}_h..

```

TextFontTypeSlider(n_

```
ItemBox{0..19}Save{n:0..20}_h...
ItemBox{0..19}Preset{n:0..20}_h..
```
多分セーブスロットとプリセット枠でセーブできるアイテムのスロット?
