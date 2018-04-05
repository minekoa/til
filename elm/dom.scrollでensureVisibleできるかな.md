# Dom.Scroll で ensureVisible できるかな？

NativeModuleで頑張ってたけれど、pure Elm でできそうな気がしてきました

[Dom.Scroll - dom 1.1.1](http://package.elm-lang.org/packages/elm-lang/dom/1.1.1/Dom-Scroll)

一応こんな感じ。（elm-package.json に `"elm-lang/dom": "1.0.0 <= v < 2.0.0"` を足すのを忘れずに）


```elm 
import Dom.Scroll as Scroll
import Debug



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        IgnoreResult ->  -- Task Result (Native)
            (model, Cmd.none)

        EnsureVisible ->
            ( model
            , Cmd.batch
                  [ Scroll.y (cursorID model)
                      |> Task.andThen (\n -> Scroll.toY (frameID model)  (Debug.log "ypos=" n))
                      |> Task.attempt (\_ -> IgnoreResult)
                  ]
--                   ensureVisible model
            )

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )
```

Domパッケージのリファレンスを読んでみたけれど、結局、DOMオブジェクトの現在位置は (JQuery の offset そうとうがいいな）をとれなさそうなので、
やっぱ無理かな。

全部計算出だせばいけるかも、とおもうので、オクラ入り。


