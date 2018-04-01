# updateループをもう一周余計に回す

たとえば、小モジュール内で親に見える update Event を加工したい場合。
（そうでない場合は、update関数内でupdate関数をもう一度呼んでしまえばいい）


以下の例は、FilesDropped を Read Fileに載せ替えたい

```elm
module Hoge exposing
    ( Model
    , Msg(ReadFile)
    , init
    , update
    , view
    )

type Msg
    =  FilesDropped (List FileReader.File)
    | ReadFile FileReader.File

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FilesDropped filelist ->
            case List.head filelist of
                Just f ->
                    ( { model | inDropZone = False }
                    , Task.perform ReadFile (Task.succeed f)
                    )
                Nothing ->
                    ( model, Cmd.none )
        ReadFile _ ->
            ( model, Cmd.none )
```

## 参考

* [Elm の update 関数を綺麗に書くための Tips | ジンジャー研究室](http://jinjor-labo.hatenablog.com/entry/2017/04/27/124105)

