# IME の入力

テキストエディタの自作は、基本的には textarea に対する入力を inputイベントで取ってきて、それを div やらなんやらで
レンダリングしてあげることに成るのだが、そうはシンプルにいかないのがIMEだ。

view が単なる textarea のミラーの場合、未確定入力自体が input されるので、それも表示に反映されるし、
変換が確定した場合は未確定入力は消えて確定されたものが入力されたように振る舞う。

ただし、テキストエディタの戦略として、inputイベントで入力を検知したら、その value はただち空にする必要がある。
これは、textarea の状態（textarea内でのカーソル位置）を把握できないので、常に1文字しかない状態にするしかないため。

しかしこの作戦は、IME入力時に、途端に素朴なイベントフックからのviewへのミラーでは成り立たなくなってします。


## 大まかな方針

鬼門は

* 未確定入力のインライン表示と、確定時にそれを消す
* 変換候補リストをちゃんとカーソルの位置にだす

の二点。


## さて、どのようにイベントを取るべきか

上記のように、IME入力中は、textarea の value を即時削除してはならず、キープする必要がある。
また、その内容は「変換プレビュー」領域に表示し、確定された文章本体への追加と混ざらないようにせねばならない。

というわけで、input イベントが変換中のものなのか、そうでないのかを見分ける必要がある。

### 方法1. keydown 229 を拾う

IME入力中は、

* keydown イベントにて 229 のコードがくる
* keypress イベントがこない　(keydownのすぐ後にkeyupがくる)

という振る舞いをする。（firefox以外）…※追記を参照

これにより keydown 229 が来た後の input イベントは「未確定入力」とみなし、変換プレビューに取り込み、また、平常処理の textarea.value を消す処理をしないようにすればいい。


一方で、や買っいなのは確定時。これについては、ブラウザにより動作がバラバラである。

* IE  .. keydown 229 → keyup 13
* Chrome, Safari .. keydown 229 → [keyupイベントが来ない！]

正直 Chrome とかだと keydown 229 でタイマーを開始して、

* 一定時間(たとえば300msecくらい？)たっても keyup が来なければ確定とみなす
* keyup なしに 別keydown がきたら確定とみなす

の作戦しかなさそう。


firefox は、keydown 0 が来て、その後確定するまで一切キーイベントがこない。確定時に keyup 13 がくる、という挙動になっている

#### 追記

[今後 IME 変換中にも keydown、keyup イベントが発生するようになります](https://www.fxsitecompat.com/ja/docs/2018/keydown-and-keyup-events-will-soon-be-fired-during-ime-composition/)

> この新たな挙動は Firefox 61 以降 Nightly と早期 Beta/DevEdition チャンネルでは既に有効化されています。

とのこと

### 方法2. CompositionEvent を使う

* `compositionstart`: IMEによるテキスト編集開始時に発火
* `compositionupdate`: IME で編集中のテキストが変更された時に発火
* `compostionend`: IMEによるテキスト編集終了時に発火

compositionstart と compositionupdate は同時に発火する。compositionupdate と compositionend は同時には発火しない。

今回はこちらを採用した。


```elm
type Msg
    = Input String
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input s ->
            case model.enableComposer of
                True ->
                    ( { model
                          | input_buffer = s
                      }
                    , Cmd.none )
                False ->
                    ( insert model (Buffer.nowCursorPos model.buffer) (String.right 1 s)
                      |> inputBufferClear
                      |> eventMemorize ("(" ++ (String.right 1 s) ++ ")")
                    , Cmd.none)
        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            compositionEnd data model

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    ( insert model (Buffer.nowCursorPos model.buffer) data
      |> composerDisable
      |> inputBufferClear
    , Cmd.none
    )

-- util

inputBufferClear : Model -> Model
inputBufferClear model =
    { model | input_buffer = "" }

composerDisable : Model -> Model
composerDisable model =
    { model
        | compositionData = Nothing
        , enableComposer = False
    }
```

```elm
               , div
                     [ style [("position", "relative"), ("display" , "inline-flex")] ]
                     [ textarea [ id <| model.id ++ "-input"
                                , onInput Input
                                , onKeyDown KeyDown
                                , onCompositionStart CompositionStart
                                , onCompositionUpdate CompositionUpdate
                                , onCompositionEnd CompositionEnd
                                , value model.input_buffer
                                , style [ ("width", "1px"), ("border", "none"), ("padding", "none"), ("margin","none"), ("outline", "none")
                                        , ("overflow", "hidden"), ("opacity", "0")
                                        , ("resize", "none")
                                        , ("position", "absolute") -- textarea のサイズは（入力を取れる状態を維持したままでは）0にできないので、カーソル位置がずれぬよう、浮かせてあげる
                                        ]
                                , spellcheck False
                                , wrap "off"
                                ]
                           []
                     , span [ style [("visibility", "hidden") ]] [compositionPreview model.compositionData]
                     , cursorView model
                     ]
```

```elm
-- Composition Event (IME)

onCompositionStart: (String -> msg) -> Attribute msg
onCompositionStart tagger =
    on "compositionstart" (Json.map tagger (Json.field "data" Json.string))

onCompositionEnd: (String -> msg) -> Attribute msg
onCompositionEnd tagger =
    on "compositionend" (Json.map tagger (Json.field "data" Json.string))

onCompositionUpdate: (String -> msg) -> Attribute msg
onCompositionUpdate tagger =
    on "compositionupdate" (Json.map tagger (Json.field "data" Json.string))
```

## IME入力中の切り分け

テキスト編集に関わる入力を受け付けるtextarea について、IME入力にまつわる特殊な制御は以下のように成る。

* IMEを介さない入力では、1文字入力を受け付けたらtextarea.value を即座に消す
    * textarea中で文字入力意外の編集（たとえばカーソル移動、削除）が行われてしまった場合に、TextEditorのモデルとの同期が非常に面倒になるため、編集の余地がない状態を維持する必要があります
* IMEで入力中は、文字入力をちょうどきっちり変換中の文字だけある状態にする
    * IME変換中の文字列が消されてしまった場合、入力が強制的にキャンセルされます。なので残しておく必要があります
    * 変換リストの表示位置制御のために、preview と textarea の文字位置は一致させる必要があります

ですので、input イベントに対し、それが

```
type 入力 
    = A.. IMEを介さない確定した入力
    | B.. IMEをつかった確定していない入力
    | C.. IMEをつかった確定入力
```

を見分ける必要があります。

compositionend を使う場合、最後の(C)で得られるdataは、そちらで取ればいいので、IMEによるもの(B||C)か、そうでない(A)かを見分ければ良いとなります。

ここで落とし穴なのですが、compositionイベントとinputイベントの関係（状態遷移モデル）は、ブラウザにより異なります。

確定をスタートに、プロセスを書くと

```
chrome:
● → (確定) →　keydown 229  → compositionUpdate s → input s → compositionEnd s

firefox:
● → (確定)                                                    →　compositionEnd s → input s
```

となります。よって、 

* compositonStart と compositionEnd に挟まれた間をIME入力中(B||C)とする作戦 → firefox がダメ
* compositonStart と compositionEnd に挟まれた間をIME変換中(B)とし、確定文字列はinputで取る作戦 → chrome がダメ

になります。

いろいろやりようはあるのですが、 

* B||C を compositionStart 〜 not (keydown 229) までの間とする作戦
* B||C を compositionStart 〜 (keypress _) までの間とする作戦

が楽そうです。Elm側で preventDefault を出し分けるには、前者の作戦は取れないので、後者がよい、と判断しました


### と、こ、ろ、が ...

なぜか IME入力が安定しません。
textarea.value の中に前回IME確定値が残ってしまったり、逆にキャンセルも確定もできない文字列が変換候補にのこってしまったり。

前者は、IME確定を次のIME入力開始で行う場合におきます。おかしいな、compositonEnd で textarea.value を消してるのに、
inputされちゃうよ？ ..※ `compositionStart s`は開始を通知するだけで、with仮文字入力は絶対に来ない。 `s` はそれにより消された文字列、と仕様にはある（が仕様通りではない実装が多いので、まだそれをアテに作れない）。

後者はわたしのLinux環境(2マシン)下では、まれに発生するのですが、Windows 環境(1マシン)下では頻度がぐっとあがりました。
これ、ブラウザが一緒でも環境（OS?それともマシンスペック?)ごとに振るまいがちがったりで、納得行かないところ。

Edge とか変換候補の位置が踊る（ブラウン運動のようです）現象がでていて、これも大変厳しいです。


 * * *

結論から書くと、これらイベントの処理を Elm 側から JavaScript 側に移し、textarea.value に対してElm側から一切触らないようにすると、
上記の不安定な振る舞いはピタッ！と止まりました。

Elm の update 関数は、イベントハンドラ中で処理されているわけではないので、想定される同期が非同期に化けると推測されます。
IME入力/非入力検出と textarea.value の制御がコレにより破綻するのでしょう。

ざんねん。


## IME入力中の切り分け --追記 2018-06-11

「その `input{isComposing=False}`、IME入力してる周の？」問題、`keypress` が発火しないことを利用して見分ける実装にしていたが、
chromeにおいて、全角スペースなどで、keypress が発火しないのに、compositionEnd も発火しない（で入力を取りこぼす）という
不具合をだしてしまった。

以下調査

### 条件

* elm-text-editor のelm側で観測
* elm-text-editor のJavaScript側では以下のフラグ管理を実施
    * `compositionStart` で on, `keypress` で OFF になるフラグで `input` を無視するか否かのフラグ管理
* その上で、elm-text-editor はJS / ELMで以下の処理を行う
    * `input`イベント
        * input 無視フラグON
            * JS: IMEの編集操作のため、`textarea.value` は触らない
            * ELM: なにもしない
        * input 無視フラグOFF
            * JS: イベントハンドラ内で、`textarea.value` をクリア
            * ELM: `event.data` を contents に反映
    * `compositionEnd` イベント
        * JS: `textarea.value` をクリア 
        * ELM: `event.data` をcontents に反映
* ブラウザのバージョン
    * Google Chrome .. 65.0.3325.181
    * Mozilla Firefox .. 60.0.1 


### 調査1. 「あいうえお」→確定

* chromeはさいごの周の、keyupがこない
* firefox の compositionEnd後のinputが空なのは、JS 側のcompositionEndイベントでtextarea.valueをクリアしているため

| Chrome      |                               | Firefox     |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|             |                               |19:38:17.953 | `keyup :13`                   |
|19:40:15.540 | `keydown :229`                |             |                               |
|             |                               |19:38:17.907 | `input (ignored):`            | 
|19:40:15.540 | `compositionend :あいうえお`  |19:38:17.907 | `compositionend :あいうえお ` |
|19:40:15.540 | `input (ignored):あいうえお`  |             |                               |
|19:40:15.540 | `compositionupdate:あいうえお`|             |                               |
|19:40:15.540 | `keydown :229`                |             |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|19:40:15.470 | `keyup :79`                   |             |                               |
|19:40:14.945 | `input (ignored):あいうえお`  |19:38:17.520 | `input (ignored):あいうえお`  |
|19:40:14.945 | `compositionupdate:あいうえお`|19:38:17.520 | `compositionupdate:あいうえお`|
|19:40:14.945 | `keydown :229`                |             |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|19:40:14.777 | `keyup :69`                   |             |                               |
|19:40:14.666 | `input (ignored):あいうえ`    |19:38:16.826 | `input (ignored):あいうえ`    |
|19:40:14.666 | `compositionupdate:あいうえ`  |19:38:16.826 | `compositionupdate:あいうえ`  |
|19:40:14.666 | `keydown :229`                |             |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|19:40:14.566 | `keyup :85`                   |             |                               |
|19:40:14.442 | `input (ignored):あいう`      |19:38:16.638 | `input (ignored):あいう`      |
|19:40:14.442 | `compositionupdate:あいう`    |19:38:16.638 | `compositionupdate:あいう`    |
|19:40:14.442 | `keydown :229`                |             |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|19:40:14.310 | `keyup :73`                   |             |                               |
|19:40:14.200 | `input (ignored):あい`        |19:38:16.400 | `input (ignored):あい`        |
|19:40:14.200 | `compositionupdate:あい`      |19:38:16.400 | `compositionupdate:あい`      |
|19:40:14.200 | `keydown :229`                |             |                               |
|-------------|-------------------------------|-------------|-------------------------------|
|19:40:13.983 | `keyup :65`                   |             |                               |
|19:40:13.854 | `input (ignored):あ`          |19:38:15.618 | `input (ignored):あ`          |
|19:40:13.854 | `compositionupdate:あ`        |19:38:15.618 | `compositionupdate:あ`        |
|19:40:13.854 | `compositoinstart:`           |19:38:15.618 | `compositoinstart:`           |
|19:40:13.854 | `keydown :229`                |             |                               |


### 調査2. -on でスペースの入力（IMEスルー）-- 全角スペース5回入力

バグ状態 (keypress がこないので、input無視フラグが落ちていない状態）でロギング

| Chrome      |                               | Firefox     |                              |
|-------------|-------------------------------|-------------|------------------------------|
|20:12:22.754 | `keyup :32`                   |20:14:18.589 | `keyup :32`                  |
|20:12:22.659 | `input (ignored):"　　　　　"`|2018-:18.465 | `input (ignored):""`         |
|             |                               |20:14:18.465 | `compositionend :"　"`       |
|             |                               |20:14:18.465 | `compositionupdate:"　"`     |
|             |                               |20:14:18.465 | `compositoinstart:`          |
|20:12:22.659 | `keydown :229`                |             |                              |
|-------------|-------------------------------|-------------|------------------------------|
|20:12:21.368 | `keyup :32`                   |20:14:17.988 | `keyup :32`                  |
|20:12:21.254 | `input (ignored):"　　　　"`  |20184:17.859 | `input (ignored):""`         |
|             |                               |20:14:17.859 | `compositionend :"　"`       |
|             |                               |20:14:17.859 | `compositionupdate:"　"`     |
|             |                               |20:14:17.858 | `compositoinstart:`          |
|20:12:21.254 | `keydown :229`                |             |                              |
|-------------|-------------------------------|-------------|------------------------------|
|20:12:20.836 | `keyup :32`                   |20:14:17.400 | `keyup :32`                  |
|20:12:20.728 | `input (ignored):"　　　"`    |20114:17.285 | `input (ignored):""`         |
|             |                               |20:14:17.285 | `compositionend :"　"`       |
|             |                               |20:14:17.285 | `compositionupdate:"　"`     |
|             |                               |20:14:17.285 | `compositoinstart:`          |
|20:12:20.727 | `keydown :229`                |             |                              |
|-------------|-------------------------------|-------------|------------------------------|
|20:12:20.145 | `keyup :32`                   |20:14:16.701 | `keyup :32`                  |
|20:12:20.340 | `input (ignored):"　　"`      |20:14:16.569 | `input (ignored):""`         |
|             |                               |20:14:16.569 | `compositionend :"　"`       |
|             |                               |20:14:16.569 | `compositionupdate:"　"`     |
|             |                               |20:14:16.569 | `compositoinstart:`          |
|20:12:20.340 | `keydown :229`                |             |                              |
|-------------|-------------------------------|-------------|------------------------------|
|20:12:19.150 | `keyup :32`                   |20:14:16.370 | `keyup :32`                  |
|20:12:19.410 | `input (ignored):"　"`        |20:14:15.915 | `input (ignored):""`         |
|             |                               |20:14:15.915 | `compositionend :"　"`       | 
|             |                               |20:14:15.915 | `compositionupdate:"　"`     |
|             |                               |20:14:15.907 | `compositoinstart:`          |
|20:12:19.410 | `keydown :229`                |             |                              |


### 対応

`compositionEnd` イベントハンドラ内で `textarea.value` をクリアすると、Firefoxで発生する直後の `input{isComposing=False}` で空文字が来る(実機動作）。
これを利用したロジックとした。

* `textarea.value` の削除するか否かの切り分けは、`input{isComposing}` イベント内で単純に `isComposing` をみて切り分ける
* elm側での確定入力判定は、
    * `compositionStart`〜`compositionEnd` 区間か？ -> t なら ignore
    * `Input s` の `s` が `String.isEmpty` か？     -> t なら ignore

elm側でinputイベントのisComposing を拾うと 改行入力時に inputイベントが発火しない現象がでているので、フラグで管理する。
(出来合いのonInputは、なにか変な処理をしているのだろうか？）

## 確定前文字列の表示

IME入力には面倒くさいことに、変換確定前に確定前文字列をインラインで表示する、という機能がある。
（視線移動を考えると当然のUXだが、めんどい）

これを本当に Modelに放り込んでしまい、候補を切り替えるたびに 削除→挿入 するというのは
厄介ですし、まだ確定してませんよ、と表示でわかるようにする必要があるので、
TextEditor のデータ構造上別管理にしておきます。

変換候補文字列は compositionUpdate イベントで取得した値を随時反映する方向でよいでしょう。

### 細かい話（あるいは将来の話）

composition previw だが、単なる文字列を表示するだけでは実は足りない。
世の中のIMEは文節にアンダーラインを引き、区切りを目地する。
矢印キーなどでその分割を変えたり出来るので、そうでないと使いにくい。

しかしeventAPI的に上記情報は取得できない。みなどうしているのだろう？


この問題に対して、Atom エディタは完全に諦めてしまっている。最初にこの問題について気がついた時「人によっては使い物にならないだろうな」と重ったのだが、
意外にもこの「諦める」作戦をとっているのが大物エディタで、にもかかわらずそれで世の中騒がないのだから、それでいいじゃないという気がしてきた。

Aceエディタは変換中だけ生のTextAreaを見せてしまう作戦に出ている。これは使い勝手はいいが見た目が少々見苦しい。VSCode も同様のしくみらしい。

* (JavaScriptでIMEの注目文節を取得したい)[https://teratail.com/questions/59212]

ちなみに、[Input Method Editor API](https://www.w3.org/TR/ime-api/)という ワーキングドラフトがあるが、chrome はまだ未対応


## 変換候補リストの表示位置制御

候補リストの表示位置を JavaScript でどうこうする方法はない。
したがって、まるでカーソル位置に候補リストが表示されているように見えるよう、
いい感じに頑張る、という作戦になる。

具体的には、背後でキーイベントを取得するための textarea を、カーソル位置に見えない状態で
置いておくという作戦になる。

これについては、カーソルを自前描画した方法のついでで実現できる。

### note:

サイズ 0 の textarea をつくれば、カーソルの直前にそれを挟むだけでいいかなと重ったのだけれども、
やってみたら、大きさ 0 には出来なかったので、position : absolute で浮かせてあげて、カーソルと重ねるようにする必要があった。

### note-2:

textarea のサイズを指定してはいけない。どうしてかというと、変換候補リストの表示位置を
textarea 内のカーソル位置をもとに計算する場合があるからだ。(Windows環境下)


## 参考

* [JavaScript とクロスブラウザでの IME event handling (2017年)](http://tanishiking24.hatenablog.com/entry/ime-event-handling-javascript)
* [JavaScript における 日本語入力 確定 (Enter) イベント](https://garafu.blogspot.jp/2015/09/javascript-ime-enter-event.html)
    * ブラウザごとのイベントの発火順などがとてもよくまとまっていて素晴らしく参考に成る。
* [jQueryでIME入力確定時にイベントを発行する | Qiita](https://qiita.com/hrdaya/items/6488d8dd3962cf35c0a0)
* [ちょっと未来のJavaScript](http://thujikun.github.io/blog/2013/12/14/ie/)
* [UI events | W3C Working Draft](https://www.w3.org/TR/DOM-Level-3-Events/#dom-compositionevent-data)
