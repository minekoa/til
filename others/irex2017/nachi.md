# NACHi (IREX2017)

{{TOC}}

## ひと協働ロボット CZ10

!{width:800px}PC016026.JPG!
!{width:800px}PC016028.JPG!

ペンダント

!{width:800px}PC016029.JPG!

ロボット手首部スティック

!{width:800px}PC016030.JPG!

### VPL & ダイレクトティーチング

!{width:800px}PC016033.JPG!

動画あり。けっこう根掘り葉掘り聞けた。


* ダイレクトティーチング
    * 手先力センサーじゃなくって、各軸のセンサーなのでどこを持ってもティーチングできる
    * 位置（姿勢キープ）、姿勢（位置キープ）、ツール（ピッキングのような動作）というティーチングモードを持つ
* VPL
    * パネルの更新ボタンで、教示位置更新
    * 述語で、「isHandOn?」があった。
    * [Google Blockly](https://developers.google.com/blockly/) ベースと言っていた
    * ロボットについているきのこスティック（プレステのあれ）は、（ロボットを動かすためではなく）ソフト上のカーソルを動かすためのもの。
        * !{width:800px}PC016031.JPG!


## モーションティーチング

!{width:800px}PC317382.JPG!
!{width:800px}PC317383.JPG!

!{width:800px}PC317384.JPG!

OpenNRをつかったアプリケーション開発の一例とのこと

1. ロボットの前でカメラの上に手をかざしてください（グーのポーズ）
2. 人差し指を立ててください（指先が認識されます）
3. そのまま指を動かしてロボットを動かしてしましょう
4. 手をひあらけばロボットとの動機は終了します
