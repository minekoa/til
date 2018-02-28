# マウスでクリックするとカーソルが動くようにする

## 後で書く

とりあえずコード

```elm
type Msg
    = Input String
          ・
          ・
    | DragStart Int Mouse.Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
          ・
          ・
        DragStart row xy ->
            let
                calc_w  = calcTextWidth (model.id ++ "-ruler")
                calc_col = (\ ln c x ->
                              if (calc_w (String.left c ln)) > x || String.length ln < c  then c - 1
                              else calc_col ln (c + 1)  x)

                ln = Buffer.line row model.buffer.contents |> Maybe.withDefault ""
                rect = getBoundingClientRect (model.id ++ "-codeLayer")

                col = (calc_col ln 0 (xy.x - rect.left))

                b1 = model.buffer
                b2 = { b1 | cursor = Buffer.Cursor row col }


            in
                ( { model | buffer = b2, xy = xy}
                  |> eventMemorize ("Ds(" ++ (toString xy.x) ++ ", " ++ (toString xy.y) ++ "{lft" ++ (toString rect.left) ++")")
                  |> eventMemorize ("CALC(" ++ ln ++ "):" ++ (toString col))
                  |> blinkBlock
                , Cmd.none )
```

線形探索。位置文字ずつ増やしながら長さを計算し、マウスで得られた座標値(X)を上回る文字数を探す。
二分探索したほうがよいかもだけれど、今のところは気にしない。
注意点は、マウスは絶対座標だから相対座評価するために、基準となる親オブジェクトのrectを取得する必要がある。

ちなみに行はオブジェクトになっているのでそれを利用して特定する。こんなイメージ。

```elm
view model =
   div [ class "line"
       , style [ ("height", "1em")
               , ("text-wrap", "none")
               , ("white-space", "pre")
               ]
       , onMouseDown (DragStart n)
       ] [text ln] ) contents

onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Json.map tagger Mouse.position)
```

文字表示幅を計算するのは、実際にレンダリングしないとわからないのでそれをやる JavaScriptをNativeモジュールに書く

```javascript
  function calcTextWidth(_id, txt) {
	  const element = document.getElementById(_id); 
      if (element == null) {
          return 0;
      }
      element.textContent = txt;
      const w = element.offsetWidth;
      element.textContent = null;

      return w
  }

  function getBoundingClientRect(_id) {
	  const element = document.getElementById(_id); 
      if (element == null) {
          return {"left":0, "top":0, "right":0, "bottom":0, "x":0, "y":0, "width":0, "height":0};
      }
      var rect = element.getBoundingClientRect();
      return rect;
  }
```

そのラッパー。

```elm
calcTextWidth : String -> String -> Int
calcTextWidth id txt = Native.Mice.calcTextWidth id txt

type alias Rect =
    { left :Int
    , top : Int
    , right: Int
    , bottom : Int
    , x : Int
    , y : Int
    , width :Int
    , height : Int
    }


getBoundingClientRect: String -> Rect
getBoundingClientRect id = Native.Mice.getBoundingClientRect id
```


## 参考

### Elm からマウスの座標を取得する

* [elm-lang/mouse/1.0.1/Mouse](http://package.elm-lang.org/packages/elm-lang/mouse/1.0.1/Mouse)
* [ドラッグのExample](http://elm-lang.org/examples/drag)

### マウスの座標（絶対座標）を早退座標に変える

* [JavaScript で DOM 要素の絶対座標を取得する方法](http://phiary.me/javascript-get-bounding-client-rect-absolute-position/)
* [JavaScript 相対的なマウス位置を取得する](http://tmlife.net/programming/javascript/javascript-mouse-pos.html)
* [element.getBoundingClientRect|MDN](https://developer.mozilla.org/ja/docs/Web/API/Element/getBoundingClientRect)

### 文字列の幅を計算する

* [javascriptで文字列の描画幅を取得する方法|Qiita](https://qiita.com/Sinraptor@github/items/1b3802db80eadf864633)


