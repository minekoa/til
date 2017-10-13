# CSS だけで リップルエフェクトをつくる


以前、Elm のイベントの懇親会で、マテリアルデザインのリップルエフェクトは、ポイのであれば、
JavaScriptを使わずに CSS だけでつくれる、という話を小耳に挟んだので、ちょっと調べてみたら

こんなページがヒットした。

* [CSS3 のみでMaterial Designライクなripple effect を試してみた](http://cartman0.hatenablog.com/entry/2015/07/11/230632)

よい。

JavaScript を使う場合

* [クリックで波紋を出すエフェクト（Ripple Effect）実装メモ　- Qiita](https://qiita.com/nekoneko-wanwan/items/c9f26ce049bd422e555c)
* [波紋が広がるリップルエフェクトボタンの作り方（CSS・Javascript）- サルワカ](https://saruwakakun.com/html-css/material/ripple_effect)

差分は、

* 波紋の中心がクリックしたポイントになるか
* 押しっぱなしのときに波紋が広がるか


## 実装

```
button {

    ・
    ・
    ・

    /* ripple effect */
    position: relative;
    overflow: hidden;
}

/* for ripple effect */
button:after {
    content: "";
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 10em; /* この値は目合わせ */
    height: 10em;
    border-radius: 50%;
    opacity: 0;
    background-color: rgba(250,250,250,0.7);
    transition: opacity 1s, width 1s, height 1s;
}

button:active:after {
    content: "";
    width: 0;
    height: 0;
    opacity: 1;
    transition: opacity 0s, width 0s, height 0s;
}
```

擬似要素 after の transition で実装しています。
キモは、after:active -> after への transition であること。



## おまけ

* [CSSだけで実装できるボタンアニメーション（マテリアルデザイン用）](https://saruwakakun.com/html-css/material/button_animation)


