# Voice Meeter Banana

* [ASIO ドライバを使って Voice Meeter Banana → DAWを設定する手順](https://www.dondari.com/ASIO_%E3%83%89%E3%83%A9%E3%82%A4%E3%83%90%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%A6_Voice_Meeter_Banana_%E2%86%92_DAW%E3%82%92%E8%A8%AD%E5%AE%9A%E3%81%99%E3%82%8B%E6%89%8B%E9%A0%86)
* [VoiceMeeter Banana備忘録](https://frailleaves.com/broadcast/vmbanana/)
* [バ美肉Zoom on Win その3](https://note.com/kuroaki/n/n2eaa1ce7c69d)
* [ステミキいらず！仮想ミキサーVoicemeeter Bananaを使ってDTM配信をしてみよう](http://vocareateproject.hatenablog.jp/entry/dtmstreamvoicemeeter)
* [DAWの音を配信する方法（Win10編）](https://note.com/kawasemihal/n/nd1db6d822aac)
* [CubaseやStudioOne等、DAWを音ありで簡単に録画する！](http://menster.wp.xdomain.jp/daw-rec/)
* [『ハードとソフトで殴ればkawaiiは創れる。』 ＃ボイチェン ノウハウ、構成まとめ【編集可】#バ美肉 2010年から現在までTweetのログあさりました。構成](https://togetter.com/li/1592493?page=3)

## 発端

DAWとかVSThostとか、ASIOドライバー使う場合、
入力も出力も オーディオインターフェイス固定になっちゃうのが
結構普通みたい。

ASIOドライバー排他もその単位でかかります。

ので、ASIOの低遅延とボイチェンと OBSへの入力・・とやろうとすると
うまく行かない

で、Voice Meeter Banana を使って実現するという流れです。

## 設定

まずHARDWERE OUTPUT1 を UR22のASIOドライバーに設定します
（するとHARDWAREINPUT1もASIOのインプットに自動的になります）

System Settings/Optionse ダイアログで
OUT A1 Main Device にて、
IN1 に 1, 2 を割り当てます。

`PATCH ISERT` の
`in1 LEFT` と `in1 Right` の `□-□` をクリックして `□┐┌□` みたいな感じにセッティング


※これでインプット1のL、R（1ch、2ch）とVoicemeeter上のアウトプット（A1,A2,A3,B1,B2）との間に音声信号を経由させる、という意味になると解釈


次にDAW側で Voicemeeter Insert Virtual ASIO を設定してあげれば
DAWの入出力がこれに挟まれるようになります。ボイチェンはココ。
