# 報告書

## プログラム作成に要した時間

* 問1 7:47:07
* 問2-i  33:06
* 問2-ii 1:43:35


## プログラム作成似あたり、気を使った点、特徴、アピールポイント

### 問1

省メモリプログラミングになることが予想されたため、
VMを必要とせず、副作用のある制御も書ける、手慣れた言語として C++17 を選択。

ログインIDとメールアドレスのユニーク照合について、std::set や std::unorderd_set を
利用し素直に作成したところ、実行時にぎりぎり 1GBを超える（あるいは近辺である）ことがわかった。

| 使用したコレクション | メモリ消費 | 処理時間 |
|----------------------|------------|----------|
|std::set              | 1099132(KB)|  2:53.94 |
|std::undordered_set   | 1018664(KB)|  2:06.43 |


* 7000000件 (1.1GB) のファイルを作成した
* 計測には time コマンドを使用
  * メモリ消費は プロセス生存中の resident set size の最大値 (%M)
  * 処理時間は経過した実時間 (%E)
* 実行環境
  * ASUS Chromebook Flip C302CA
  * CPU Model: Intel(R) Core(TM) m3-6Y30 CPU @ 0.90GHz
  * MemTotal: 3938560 kB
  * ストレージ: eMMC
  * Ubuntu 16.04 LTS on crouton


コレクションクラスにおいて省メモリになるようなアイデアは、
省メモリな単射を得るためのアルゴリズム（1byte に複数文字をエンコードするなど) を試したが、
今回試行したものでは思うような成果は得られず、
また、正しく動作することを検証するのに時間がかかることが予想されたため、この方向をとりやめ。

愚直であるが、ユニークな ログインID と ユニークなメールアドレスをそれぞれ
順番に作り置きして一時ファイルに保存し、CSVファイル作成時に読み込みすることで、
ユニーク照合データの並列保持をやめ、メモリ消費の最大量を抑えることとした。

最終版の計測結果は以下の通り

| メモリ消費 | 処理時間 |
|------------|----------|
|  512372(KB)|  2:31.33 |


### 問2-i

特にメモリ消費量や実効速度を考慮する必要がない仕様であるため、
手早く実装できる Python3 で作成。

| メモリ消費 | 処理時間 |
|------------|----------|
|   19324(KB)|  0:27.10 |


### 問2-ii

Python3 で実装したのち、性能不足解決のため C++ で再実装した。

単純に一つのファイルを1レコードずつ読み込みながら、
もうひとつのファイルを頭から検索するアルゴリズムは、
メモリ上にストアする情報がないため消費メモリ面で大変有利な一方、
700万回ファイルを操作するため、処理時間が長大になるため、採用できない。

よって一度、片方のファイルを走査し、ログインID と そのレコードの頭への シークオフセット の連想配列を作成し、
メモリ消費量と処理時間のバランスをとった。

上記アルゴリズムに基づき、 Python3 で実装したものの、前述の通り
メモリ消費量が 1GBを超えてしまったため、問1の結果より500MB前後の消費で実行できると予想される
C++ での再実装を行った。

計測結果は以下の通り。


| バージョン     | メモリ消費 | 処理時間 |
|----------------|------------|----------|
| Python版       | 1443232(KB)|  2:26.96 |
| C++版 (最終版) |  509560(KB)|  2:19.84 |


―以上―

