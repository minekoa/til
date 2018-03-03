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

という振る舞いをする。（firefox以外）

これにより keydown 229 が来た後の input イベントは「未確定入力」とみなし、変換プレビューに取り込み、また、平常処理の textarea.value を消す処理をしないようにすればいい。


一方で、や買っいなのは確定時。これについては、ブラウザにより動作がバラバラである。

* IE  .. keydown 229 → keyup 13
* Chrome, Safari .. keydown 229 → [keyupイベントが来ない！]

正直 Chrome とかだと keydown 229 でタイマーを開始して、

* 一定時間(たとえば300msecくらい？)たっても keyup が来なければ確定とみなす
* keyup なしに 別keydown がきたら確定とみなす

の作戦しかなさそう。


firefox は、keydown 0 が来て、その後確定するまで一切キーイベントがこない。確定時に keyup 13 がくる、という挙動になっている

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

## 候補リストの表示

候補リストの表示位置を JavaScript でどうこうする方法はない。
したがって、まるでカーソル位置に候補リストが表示されているように見えるよう、
いい感じに頑張る、という作戦になる。

具体的には、背後でキーイベントを取得するための textarea を、カーソル位置に見えない状態で
置いておくという作戦になる。

これについては、カーソルを自前描画した方法のついでで実現できる。

note:

サイズ 0 の textarea をつくれば、カーソルの直前にそれを挟むだけでいいかなと重ったのだけれども、
やってみたら、大きさ 0 には出来なかったので、position : absolute で浮かせてあげて、カーソルと重ねるようにする必要があった。

## 細かい話

composition previw だが、単なる文字列を表示するだけでは実は足りない。
世の中のIMEは文節にアンダーラインを引き、区切りを目地する。
矢印キーなどでその分割を変えたり出来るので、そうでないと使いにくい。

しかしeventAPI的に上記情報は取得できない。みなどうしているのだろう？


この問題に対して、Atom エディタは完全に諦めてしまっている。最初にこの問題について気がついた時「人によっては使い物にならないだろうな」と重ったのだが、
意外にもこの「諦める」作戦をとっているのが大物エディタで、にもかかわらずそれで世の中騒がないのだから、それでいいじゃないという気がしてきた。

Aceエディタは変換中だけ生のTextAreaを見せてしまう作戦に出ている。これは使い勝手はいいが見た目が少々見苦しい。VSCode も同様のしくみらしい。

* (JavaScriptでIMEの注目文節を取得したい)[https://teratail.com/questions/59212]

ちなみに、[Input Method Editor API](https://www.w3.org/TR/ime-api/)という ワーキングドラフトがあるが、chrome はまだ未対応

## 参考

* [JavaScript とクロスブラウザでの IME event handling (2017年)](http://tanishiking24.hatenablog.com/entry/ime-event-handling-javascript)
* [JavaScript における 日本語入力 確定 (Enter) イベント](https://garafu.blogspot.jp/2015/09/javascript-ime-enter-event.html)
    * ブラウザごとのイベントの発火順などがとてもよくまとまっていて素晴らしく参考に成る。
* [jQueryでIME入力確定時にイベントを発行する | Qiita](https://qiita.com/hrdaya/items/6488d8dd3962cf35c0a0)
* [ちょっと未来のJavaScript](http://thujikun.github.io/blog/2013/12/14/ie/)
* [UI events | W3C Working Draft](https://www.w3.org/TR/DOM-Level-3-Events/#dom-compositionevent-data)
