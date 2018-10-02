# Copy and Paste

## 結論

ライブラリとしての実現は不可能

Clipboard API は、 `e.clipboarddata` として DataTransferオブジェクトが帰ってくるのだが、
Elm側からこれを操作することができないため。

( JavaScript であれば `.getData( "text/plane")`、`.setData("text/plane", "HOGEHOGE")` でクリップボードの内容を操作できる )


アプリケーションであるならば、CustomElement を作成し、それを Elm側から操作する方法でいけるっぽい
(Portの場合は、一度イベントハンドラから出てしまっているのでダメだし、そもそもイベントオブジェクトをゲットできない）

* [How to use the Synchronous Clipboard API without native code?](https://discourse.elm-lang.org/t/how-to-use-the-synchronous-clipboard-api-without-native-code/878/9)
