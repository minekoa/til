# カーソル移動でスクロールを行う

## 作戦

> ## 特定要素までのスクロール到達判定
> 
> jQueryなら`offset().top`で取れるね。
> 
> ```javascript
> // 要素の位置を取得
> var targetTop = $elTarget.offset().top;
> 
> $(window).on('scroll', function(event){
>     // elScrollableは別項「スクロール位置の取得」を参照
>     var scrollTop = elScrollable.scrollTop;
>     if (scrollTop >= targetTop) {
>         // 要素到達時の処理
>     }
> });
> ```
> 
> 画面最下部に要素が出てきたとき、なら上記で。画面中央とかまで待ちたいのなら、前述のwindowHeightを使って条件を調整してやる。
>
>> [スクロールで何かするための処理まとめ。（JavaScriptおれおれAdvent Calendar 2014 – 21日目）](https://ginpen.com/2014/12/21/onscroll/)

を、JQuery なしで行う。

というわけで…

> ## .offset()（top 値を取得）
>
> jQuery
> 
> ```javascript
> var myTop = $("#main").offset().top
> ```
> 
> 書き換え JavaScript
> 
> ```javascript
> var myMain = document.getElementById("main");
> var rect = myMain.getBoundingClientRect();
> var scrollTop = window.pageYOffset || document.documentElement.scrollTop;
> var myTop = rect.top + scrollTop;
> ```
> 
> ものすごく複雑です。myTop に「top 位置」が入ります。
> 
> offsetは画面の左上からの距離を取得しますが、それを一発で出す命令は存在しないので、難しい計算をします。jQuery もほぼこのようなコードで動いています。
>
>> [脱脱jQuery .height() .width() .offset() .scrollTop() | q-Az](https://q-az.net/without-jquery-height-width-offset-scrolltop/)</cite>

うは、めんどくさ

ちなみに単位は `pixel : Int` 。

`element.scrollTop` と `element.scrollLeft` に値を代入すれば反映される

* [HTML DOM scrollTop Property](https://www.w3schools.com/jsreF/prop_element_scrolltop.asp)

## 参考

* [DIVのスクロールをJavaScriptから操作する](http://d.hatena.ne.jp/yoheiM/touch/20110721)
* [脱脱jQuery .height() .width() .offset() .scrollTop() | q-Az](https://q-az.net/without-jquery-height-width-offset-scrolltop/)
* [Element.scrollTop - 要素内の垂直方向のスクロール量を取得、変更する](https://syncer.jp/javascript-reference/element/scrolltop)
* [スクロールの位置（量）を取得・設定する](http://shanabrian.com/web/javascript/get-scroll-position.php)
* [JavaScriptで要素のスクロール値を取得して背景色に反映させてみよう](http://phiary.me/javascript-scroll-background-color)
* [スクロールで何かするための処理まとめ。（JavaScriptおれおれAdvent Calendar 2014 – 21日目）](https://ginpen.com/2014/12/21/onscroll/)
    * 特定要素までのスクロール判定
