# セシルちゃんツールズのの設定、どこにセーブされてるの？

セシル変身などは、exeを別フォルダに移動しても（※バージョンアップ時など）
作成したアバターの設定は受け継がれる。

また、セシル変身の [File] メニューに 「PlayerPrefs.DeleteAll()を実行します。アプリデータ自体が消えてしまうので注意してください」
という注意書きのある [DeleteData] ボタンがあり、
このことから レジストリにセーブデータがあることが予想される。

[UnityEngine.PlayerPrifs](https://docs.unity3d.com/ja/2018.4/ScriptReference/PlayerPrefs.html)

> On Windows, PlayerPrefs are stored in the registry under HKCU\Software\[company name]\[product name] key, where company and product names are the names set up in Project Settings.


## セシル変身

`\HKEY_CURRENT_USER\SOFTWARE\DreamFactory\CecilHenSin` にある。

セーブデータは多分

```
"EasySave0_h2792951642"=hex:30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2e,31,33,34,38,38,38,34,2c,30,2e,32,35,37,33,35,32,39,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2e,30,34,33,\
  31,33,37,32,35,2c,30,2e,31,36,30,37,38,34,33,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2e,31,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,31,2c,30,2e,34,2c,30,2c,30,2c,\
  30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,2c,30,\
  2c,30,2c,30,2c,30,2c,30,00
```

かなぁ。バイナリなのでよくわからんです。包含しているはずの編集髪のデータと比べると
ちょっと小さい気がする。

セシル変身はエスクポート・インポート機能が追加されているので、まぁ、そこまで深刻では。

まるごとバックアップして新しいマシンにつれていけばOKな気がする。

## ItemShop

`\HKEY_CURRENT_USER\SOFTWARE\DreamFactory\ItemShop` にある。
恐ろしい数のキーがあるので正直良くわかんない。


`ItemBox<N>Save<M>_hxxxxxxxxxx`というキーがセーブスロットMに保存されているアイテムボックスNの値？
設定されていないスロットが ffffffff になってるので、対応付はあってるはずだけど、
アイテム一つ分の設定値が dword 一個分なのは足りないので、選んだアイテムの種類だけを保持してるのかな。
同一アイテムを選ぶと同一値になってるのでたぶんそう。

`ItemSizeXSlider<N>Item<M> _hxxxxxxxxxx` みたいなのが大量にならんでいるので、そういうことかな？

データが構造化されていないので、これのエクスポートは骨が折れそう。