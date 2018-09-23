# [CSS] pure css で ripple effect

前に別バージョン（擬似要素をborder-radios で丸にして重ねて、transitionでサイズかえる）で実装したことあるけれど、
こちら

[Pure CSS ripple effect for Material Design](https://codepen.io/finnhvman/pen/jLXKJw)

の方法のほうがシンプルで賢い。

background にradial-gradient　（円形グラデーション）を center/15000%; (つまり、グラデーションが見えないくらい大きいサイズ）で　`:hover` 時に重ねて、`:active` で 100% にする。で `transition: background 0.8s` で徐々に戻す。 



