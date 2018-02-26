# カーソル位置を計算する

## 定石

カーソル前までの文字列の長さを計算する方法はないので、一度、表示部と全く同じスタイルを適用した要素を用意して、その長さを測定する、という方法をとる

```html
<span id="ruler" style="visibility:hidden;position:absolute;white-space:nowrap;"></span>
```

これの `offsetwidth` を取得する。

JavaScriptだと関数になるけれど、Elmだと一度レンダリングさせてから、なんやのイベントをおこさなければならなさそうで、面倒くさそう。


## これで出来るんじゃね？

単純に

```html
<div class="line" style="position:relative"> 
    <div class="code-layer" style="position:absolute">hogehoge piyopiyo</code>
    <div class="cursor-layer"style="position:absolute; display:flex; flex-flow:row nowrap">
        <span class="ruler-pad" style="positon:relative; visibility:hidden; white-space:pre">hogeho</span>
        <span class="cursor" style="positon:relative; background-color:blue; border:none; width:2px"></span>
    </div>
</div>
```

でいいんじゃね？

やってみて今のところ問題がない。けれど、定石になっていない理由はありそうなので、要注意（落とし穴があるかも）。

## Tip

カーソル直前までがスペースであるとき、以下のスタイルを適用しないと、その分が縮められてしまう

```
("white-space", "pre")
```

## 参考

* [javascriptで文字列の描画幅を取得する方法 | Qiita](https://qiita.com/Sinraptor@github/items/1b3802db80eadf864633)

