# カスタムイベント

```javascript
    function elaborateInputAreaEventHandlers(id_input_area) {
        const input_area = document.getElementById(id_input_area);

        input_area.addEventListener( "paste", e => {
            e.preventDefault();

            const data_transfer = (e.clipboardData) || (window.clipboardData);
            const str = data_transfer.getData("text/plain");
            console.log("paste");

            const evt = new CustomEvent("pasted", { "bubbles": true,
                                                    "cancelable": true,
                                                    "detail": str
                                                  }
                                       );
            input_area.dispatchEvent(evt);
        });

        input_area.addEventListener( "copy", e => {
            e.preventDefault();

            const str = input_area.selecteddata
            e.clipboardData.setData('text/plain', str);
            console.log("copy");

            const evt = new CustomEvent("copied", { "bubbles": true,
                                                    "cancelable": true,
                                                    "detail": str
                                                  }
                                       );
            input_area.dispatchEvent(evt);
        });
    }
```

```elm
type Msg
    | Pasted String
    | Copied String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Pasted s ->
            ( {model | buffer = Buffer.insert s model.buffer }
                  |> eventLog "pasete" s
            , Cmd.none
            )
        Copied s ->
            ( { model | copyStore = s }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div
        [ style [("position", "relative"), ("display" , "inline-flex")] ]
        [ textarea [ id <| model.id ++ "-input"
                   , onPasted Pasted
                   , onCopied Copied
                   , value model.input_buffer
                   , property "selecteddata" <| Json.Encode.string <|
                         case model.buffer.selection of
                             Nothing -> ""
                             Just sel -> Buffer.readRange sel model.buffer
                   ]
                   []
        ]

onPasted: (String -> msg) -> Attribute msg
onPasted tagger =
    on "pasted" (Json.map tagger (Json.field "detail" Json.string))

onCopied: (String -> msg) -> Attribute msg
onCopied tagger =
    on "copied" (Json.map tagger (Json.field "detail" Json.string))
```

* CustomEvent は、 `detail` 要素以下にカスタムな情報をもたせる
* 普通にElmから`on`で補足できる

このパターンを使えば、JavaScript 側でゴニョゴニョやってから Elm にそれを通知することで同期できる。
（ただし、Elm側の Model は参照できないので、propertyなどに落とし込まねばならない）

イベントリスナーの登録は、本当にちゃんと作るならば init時にbody要素に登録し、
イベント引数からtargetのIDをみて処理を行うようにすれば、
要素が破棄され、VirtualDOMから再生性されるようなことになっても大丈夫なふうに作れる。

## 参照

* [Custom Event | MDN](https://developer.mozilla.org/ja/docs/Web/API/CustomEvent)
* [イベントの発火と作成 | MDN](https://developer.mozilla.org/ja/docs/Web/Guide/Events/Creating_and_triggering_events)
* [javascript：カスタムイベントを使ったイベントハンドラの実装 | Qiita](https://qiita.com/tbtbt/items/2fbef055dbfd29c7158e)
* [JavaScriptのカスタムイベント | console.realog()](https://lealog.hateblo.jp/entry/2014/01/11/193457)

