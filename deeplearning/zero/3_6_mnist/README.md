
## PIL のインストール

Python Image Library のインストール。

今は開発が止まっているため、 フォークされている pillow をインストールする

```
pip install pillow
```

### 参考

https://note.nkmk.me/python-pillow-basic/



## sample_weight.pkl

学習済みのネットワーク `src/sample_weight.pkl` は
https://github.com/oreilly-japan/deep-learning-from-scratch/blob/master/ch03/sample_weight.pkl
より取得した。


## 3章まとめ

* ニューラルネットワークでは、活性化関数としてシグモイド関数やReLU関数のようななめらかに変化する関数を利用する
* NumPyの多次元配列を上手く使うことで、ニューラルネットワークを効率よく実装できる
* 機械学習の問題は、回帰問題と分類問題に大別できる
* 主力層で仕様する活性化関数は、回帰問題ではid関数、分類問題ではソフトマックス関数を一般的に利用する
* 分類問題では、出力層のニューロンの数を分類するクラス数に設定する
* 入力データの集まりをバッチといい、バッチ単位で推論処理をすることで、計算を高速に行うことが出来る
