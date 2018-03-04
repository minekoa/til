module Editor.Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse
import Time exposing (Time, second)


import Editor.Buffer as Buffer
import Native.Mice

{-| This module is simple texteditor.

-}


type alias Model =
    { id : String -- frame id

    , buffer : Buffer.Model

    -- ?
    , selection : Maybe Buffer.Range
    , copyStore : String

    -- frame
    , input_buffer : String
    , enableComposer : Bool
    , compositionData : Maybe String --IMEで返還中の未確定文字
    , focus : Bool
    , blink : BlinkState

    -- for debug
    , event_memo : List String
    , xy : Mouse.Position
    , isPreventedDefaultKeyShortcut : Bool
    }

init : String -> String -> Model
init id text =
    Model id                     -- id
          (Buffer.init text)
          Nothing                -- selection
          ""                     -- copyStore
          ""                     -- input_buffer
          False Nothing          -- COMPOSER STATE
          False                  -- focus
          BlinkBlocked           -- blink

          []                     -- event_memo
          (Mouse.Position 0 0)
          False                  --preventedKeyShortcut

-- frame > cursor blink

type BlinkState
    = Blink Bool
    | BlinkBlocked


blinkTransition : BlinkState -> BlinkState
blinkTransition blnk =
    case blnk of
        BlinkBlocked -> Blink True
        Blink True   -> Blink False
        Blink False  -> Blink True


blinkBlock : Model -> Model
blinkBlock model =
    {model | blink = BlinkBlocked}



------------------------------------------------------------
-- update
------------------------------------------------------------

type Msg
    = Input String
    | KeyDown KeyboardEvent
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String
    | FocusIn Bool
    | FocusOut Bool
    | SetFocus
    | DragStart Int Mouse.Position
    | Tick Time

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

        KeyDown code ->
            keyDown code model

        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            compositionEnd data model

        FocusIn _ ->
            ( {model
                  | focus = True
                  , isPreventedDefaultKeyShortcut = if model.isPreventedDefaultKeyShortcut
                                                    then model.isPreventedDefaultKeyShortcut
                                                    else setPreventDefaultKeyShortcut (model.id ++ "-input") -- todo: タスクなりにしないとなー。initのタイミングだとdomが出来ていない問題もある。とりあえずの実装。
              }
            , Cmd.none)
        FocusOut _ ->
            ( {model|focus = False}
            , Cmd.none)
        SetFocus ->
            ( {model|focus = doFocus (model.id ++ "-input")}
            , Cmd.none )

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

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )





keymapper : (Bool, Bool, Bool, Int) -> (Model -> Model)
keymapper (ctrl, alt, shift, keycode) =
    let
        keymap = [ {ctrl=False, alt=False, shift=False, code= 37, f=moveBackward } -- '←'
                 , {ctrl=False, alt=False, shift=False, code= 38, f=movePrevios }  -- '↑'
                 , {ctrl=False, alt=False, shift=False, code= 39, f=moveForward }  -- '→'
                 , {ctrl=False, alt=False, shift=False, code= 40, f=moveNext }     -- '↓'

                 , {ctrl=False, alt=False, shift=True , code= 37, f=selectBackward } -- 'S-←'
                 , {ctrl=False, alt=False, shift=True , code= 38, f=selectPrevios }  -- 'S-↑'
                 , {ctrl=False, alt=False, shift=True , code= 39, f=selectForward }  -- 'S-→'
                 , {ctrl=False, alt=False, shift=True , code= 40, f=selectNext }     -- 'S-↓' 

--                 , {ctrl=False, alt=False, shift=Nothihg, code=  8, f=(\m -> backspace m (Buffer.nowCursorPos m.buffer)) }
                 , {ctrl=False, alt=False, shift=False, code=  8, f=(\m -> backspace m (Buffer.nowCursorPos m.buffer)) } -- BS
                 , {ctrl=False, alt=False, shift=False, code= 46, f=(\m -> case m.selection of
                                                                               Nothing -> delete m (Buffer.nowCursorPos m.buffer)
                                                                               Just s  -> deleteRange m s 
                                                                    )  }   -- DEL

                 , {ctrl=True , alt=False, shift=False, code= 67, f=(\m -> m.selection |> Maybe.andThen (\sel -> Just <| copy m sel) |> Maybe.withDefault m) } -- 'C-c'
                 , {ctrl=True , alt=False, shift=False, code= 88, f=(\m -> m.selection |> Maybe.andThen (\sel -> Just <| cut m sel) |> Maybe.withDefault m) } -- 'C-x'
                 , {ctrl=True , alt=False, shift=False, code= 86, f=(\m -> pasete m (Buffer.nowCursorPos m.buffer) m.copyStore)} -- 'C-v'
                 , {ctrl=True , alt=False, shift=False, code= 90, f= undo }

                 -- emacs like binds
                 , {ctrl=True , alt=False, shift=False, code= 70, f=moveForward } --  'C-f'
                 , {ctrl=True , alt=False, shift=False, code= 66, f=moveBackward }--  'C-b'
                 , {ctrl=True , alt=False, shift=False, code= 78, f=moveNext }    --  'C-n'
                 , {ctrl=True , alt=False, shift=False, code= 80, f=movePrevios } --  'C-p'
                 , {ctrl=True , alt=False, shift=False, code= 72, f=(\m -> backspace m (Buffer.nowCursorPos m.buffer)) }  -- 'C-h'
                 , {ctrl=True , alt=False, shift=False, code= 68, f=(\m -> case m.selection of
                                                                               Nothing -> delete m (Buffer.nowCursorPos m.buffer)
                                                                               Just s  -> deleteRange m s
                                                                    )  }    -- 'C-d'
                 , {ctrl=True , alt=False, shift=False, code= 77, f=(\m -> insert m (Buffer.nowCursorPos m.buffer) "\n") }-- 'C-m'
                 ]

        search = (\ (ctrl, alt, shift, keycode) l ->
                      case l of
                          [] ->
                              Nothing
                          x :: xs ->
                              if (keycode == x.code)
                                  && (ctrl == x.ctrl) && (alt == x.alt) && (shift == x.shift)
                              then Just x.f
                              else search (ctrl, alt, shift, keycode) xs
                 )
    in
        -- note: 4つ以上の組でのパターンマッチを記述するとビルド時間が非常に長くなる問題があるので、(elm 0.18)
        --       パターンマッチでは書かない

        -- note: Dict にすべきか悩んだが、
        --       モディファイヤ * を導入するかも、なのと
        --       複数のkeymap を合成して早いものがちマッチとかやるかも、なのと
        --       nストロークキーマッチをやるかも、なので
        --       線形サーチで書いている
        search (ctrl, alt, shift, keycode) keymap
            |> Maybe.withDefault (\m -> m)
                                  

keyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
keyDown e model =
    ( keymapper (e.ctrlKey, e.altKey, e.shiftKey, e.keyCode) model
      |> eventMemorize ("D:" ++ keyboarEvent_toString e)
    , Cmd.none )

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
      |> inputBufferClear -- 直前にenterでない確定をした場合向け
      |> blinkBlock
      |> eventMemorize ("Cs{" ++ data ++ "} ")
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
      |> blinkBlock
      |> eventMemorize ("Cu{" ++ data ++ "} ")
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    ( insert model (Buffer.nowCursorPos model.buffer) data
      |> composerDisable
      |> inputBufferClear
      |> blinkBlock
      |> eventMemorize ("Ce{" ++ data ++ "} ")
    , Cmd.none
    )

------------------------------------------------------------
-- control state update
------------------------------------------------------------

eventMemorize : String -> Model -> Model
eventMemorize s model =
    { model | event_memo = (s :: model.event_memo) }

inputBufferClear : Model -> Model
inputBufferClear model =
    { model | input_buffer = "" }

composerDisable : Model -> Model
composerDisable model =
    { model
        | compositionData = Nothing
        , enableComposer = False
    }

------------------------------------------------------------
-- cursor
------------------------------------------------------------

moveForward : Model -> Model
moveForward model =
    { model | buffer = Buffer.moveForward model.buffer }
        |> selectionClear
        |> blinkBlock

moveBackward : Model -> Model
moveBackward model =
    { model | buffer = Buffer.moveBackward model.buffer }
        |> selectionClear
        |> blinkBlock

movePrevios : Model -> Model
movePrevios model =
    { model | buffer = Buffer.movePrevios model.buffer }
        |> selectionClear
        |> blinkBlock

moveNext : Model -> Model
moveNext model =
    { model | buffer = Buffer.moveNext model.buffer }
        |> selectionClear
        |> blinkBlock


------------------------------------------------------------
-- selection
------------------------------------------------------------

selection_update: (Int, Int) -> Buffer.Range -> Buffer.Range
selection_update (row, col) sel =
    {sel| end = (row, col)}

markSetIfNothing : Model -> Model
markSetIfNothing model =
    let
        pos = Buffer.nowCursorPos model.buffer
    in
        case model.selection of
            Nothing ->
                { model | selection = Just <| Buffer.Range pos pos }
            Just sel ->
                model

selectionUpdate : Model -> Model
selectionUpdate model =
    let
        row = model.buffer.cursor.row
        col =  model.buffer.cursor.column
    in
        { model | selection = model.selection |> Maybe.andThen (selection_update (row, col) >> Just) }

selectionClear : Model -> Model
selectionClear model =
    { model | selection = Nothing }


selectBackward: Model -> Model
selectBackward model =
    model
        |> markSetIfNothing
        |> (\ m -> { m | buffer = Buffer.moveBackward m.buffer })
        |> selectionUpdate
        |> blinkBlock

selectForward: Model -> Model
selectForward model =
    model
        |> markSetIfNothing
        |> (\m -> { m | buffer = Buffer.moveForward m.buffer })
        |> selectionUpdate
        |> blinkBlock

selectPrevios: Model -> Model
selectPrevios model =
    model
        |> markSetIfNothing
        |> (\m -> { m | buffer = Buffer.movePrevios m.buffer })
        |> selectionUpdate
        |> blinkBlock

selectNext: Model -> Model
selectNext model =
    model
        |> markSetIfNothing
        |> (\m -> { m | buffer = Buffer.moveNext m.buffer })
        |> selectionUpdate
        |> blinkBlock


------------------------------------------------------------
-- edit
------------------------------------------------------------

insert: Model -> (Int, Int)  -> String -> Model
insert model (row, col) text =
    { model | buffer = Buffer.insert (row, col) text model.buffer }
    |> selectionClear
    |> blinkBlock


backspace: Model -> (Int, Int) -> Model
backspace model (row, col) =
    { model | buffer = Buffer.backspace (row, col) model.buffer}
    |> selectionClear
    |> blinkBlock

delete: Model -> (Int, Int) -> Model
delete model (row, col) =
    { model | buffer = Buffer.delete (row, col) model.buffer}
    |> selectionClear
    |> blinkBlock

deleteRange: Model -> Buffer.Range -> Model
deleteRange model selection =
    { model | buffer = Buffer.deleteRange selection model.buffer}
    |> selectionClear
    |> blinkBlock

undo : Model -> Model
undo model =
    { model | buffer = Buffer.undo model.buffer }
    |> selectionClear
    |> blinkBlock

copy : Model -> Buffer.Range -> Model
copy model selection =
    -- todo: クリップボード連携(クリップボードへ保存）
    { model | copyStore = Buffer.readRange selection model.buffer }
    |> selectionClear
    |> blinkBlock

cut : Model -> Buffer.Range -> Model
cut model selection =
    -- todo: クリップボード連携(クリップボードへ保存）
    { model | copyStore = Buffer.readRange selection model.buffer
            , buffer = Buffer.deleteRange selection model.buffer
    }
    |> selectionClear
    |> blinkBlock

pasete : Model -> (Int, Int) -> String -> Model
pasete model (row, col) text =
    -- todo: クリップボード連携（クリップボードから取得、copyStoreのアップデート）、selectされていたらselectoinのdelete
    { model | buffer = Buffer.insert (row, col) text model.buffer }
    |> selectionClear
    |> blinkBlock


------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [class "editor"]
        [ presentation model ]

presentation : Model -> Html Msg
presentation model =
    div [ style [ ("display", "flex"), ("flex-direction", "row"), ("flex-wrap", "no-wrap")
                , ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("position", "relative")
                ]
        , onFocusIn FocusIn
        , onFocusOut FocusOut
        , onClick SetFocus
--        , contenteditable True
        ]
        [ lineNumArea model
        , codeArea model
        ]

lineNumArea : Model -> Html Msg
lineNumArea model =
    let
        contents = model.buffer.contents
    in
        div [ class "line-num-area"
            , style [ ("text-align", "right")
                    , ("padding-right", "0.2em")]
            ] <|
            List.map
                (λ n -> div [ class "line-num"
                             , style [ ("height", "1em")
                                     , ("text-wrap", "none")]
                             ] [ text (toString n) ])
                (List.range 1 (List.length contents))

codeArea : Model -> Html Msg
codeArea model =
    div [ class "code-area" ]
        [ ruler (model.id ++ "-ruler")
        , cursorLayer2 model
        , markerLayer model
        , codeLayer model
        ]

codeLayer: Model  -> Html Msg
codeLayer model = 
    let
        contents = model.buffer.contents
        cursor = model.buffer.cursor
    in
        div [ id (model.id ++ "-codeLayer")
            , class "lines"] <|
            List.indexedMap
                (λ n ln ->
                      if n == cursor.row then
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
--                                      , ("display" , "inline-flex")
                                      ]
                              , onMouseDown (DragStart n)
                              ]
                              [ span [ style [ ("position", "relative")
                                             , ("white-space", "pre")
                                             ]
                                     ]
                                    [ text <| String.left cursor.column ln]
                              , compositionPreview model.compositionData
                              , span [ style [ ("position", "relative")
                                             , ("white-space", "pre")
                                             ]
                                     ]
                                    [ text <| String.dropLeft cursor.column ln]                                  
                              ]
                      else
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
                                      ]
                              , onMouseDown (DragStart n)
                              ] [text ln] ) contents

cursorLayer2 : Model -> Html Msg
cursorLayer2 model =
    {- 作戦2, [パディング用要素、本物の文字を入れ、hiddenにする][cursol]
       思いついたのでやってみたらうまく行った
       けど、みんなこれをやってないの、なにか落とし穴あるからなのかな？
    -}
    div [ class "cursor-layer"
        , style [("position", "absolute")]
        ]
        [ div [style [ ("position", "relative")
                     , ("display" , "inline-flex")
                     , ("flex-direction", "row")
                     , ("flex-wrap", "nowrap")
                     , ("justify-content", "flex-start")
                     , ("height", "1em")

                     , ("top" , (model.buffer.cursor.row |> toString) ++ "em")
                     , ("left", "0")
                     ]
               ]
               [ pad model
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
               ]
        ]

markerLayer: Model -> Html Msg
markerLayer model =
    case model.selection of
        Nothing ->
            text ""

        Just sel ->
            let
                bpos = if (Buffer.isPreviosPos sel.begin sel.end) then sel.begin else sel.end
                epos = if (Buffer.isPreviosPos sel.begin sel.end) then sel.end else sel.begin

                ms = List.range (Tuple.first bpos) (Tuple.first epos)
                   |> List.map (\ r ->
                                    let
                                        cb = if r == (Tuple.first bpos)
                                             then bpos |> Tuple.second
                                             else 0
                                        ce = if r == (Tuple.first epos)
                                             then epos |> Tuple.second
                                             else String.length <| (Buffer.line r model.buffer.contents |> Maybe.withDefault "")
                                    in
                                        {row =r, begin_col = cb, end_col = ce}
                               )
            in
                div [ class "marker-layer"
                    , style [("position", "absolute")]
                    ]
                    ( List.map (\ m ->
                                  div [ style [ ("position", "absolute")
                                              , ("display", "inline-flex")
                                              , ("top" , (m.row |> toString) ++ "em")
                                              , ("left", "0")
                                              ]
                                      ]
                                      [ padToCursor (m.row, m.begin_col) model
                                      , div [ class "selection"
                                            , style [ ("background-color", "blue")
                                                    , ("color","white")
                                                    , ("white-space", "pre")
                                                    , ("border","none"), ("padding", "none"), ("margin", "none")
                                                    ]
                                            ]
                                            [ Buffer.line m.row model.buffer.contents |> Maybe.withDefault ""
                                              |> String.dropLeft m.begin_col
                                              |> String.left (m.end_col - m.begin_col)
                                              |> text
                                            ]
                                      ]
                             ) ms )

pad : Model -> Html msg
pad model =
    let
        cur      = model.buffer.cursor
        contents = model.buffer.contents
    in
    span [ class "pad"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ Buffer.line cur.row contents |> Maybe.withDefault "" |> String.left cur.column |> text ]

padToCursor : (Int, Int) -> Model -> Html msg
padToCursor pos model =
    let
        contents = model.buffer.contents
    in
    span [ class "pad"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ Buffer.line (Tuple.first pos) contents |> Maybe.withDefault "" |> String.left (Tuple.second pos) |> text ]



ruler : String -> Html msg
ruler base_id = 
    span [ id base_id
         , style [ ("position", "absolute")
                 , ("white-space", "pre")
                 , ("color", "green")
                 ]
         ]
         []

compositionPreview : Maybe String -> Html msg
compositionPreview compositionData =
    case compositionData of
        Just s ->
            span [ class "composition_data"
                 , style [ ("color", "blue")
                         , ("text-decoration", "underline")
                         ]
                 ] [ text s ]
        Nothing ->
            text ""

cursorView : Model -> Html msg
cursorView model =
    let
        blink_off = (\blnk -> case blnk of
                                Blink b      -> b
                                BlinkBlocked -> True
                    ) >> not
    in
    span [style [ ("background-color", if model.focus then "blue" else "gray" )
                , ("opacity", if model.focus && (blink_off model.blink) then "0.0" else "0.5")
                , ("height", "1em")
                , ("width", "3px")
                ]
         ]
    []


------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch [ Time.every (0.5 * second) Tick ]


------------------------------------------------------------
-- html events (extra)
------------------------------------------------------------

-- Keyboard Event

type alias KeyboardEvent = 
    { altKey : Bool
    , ctrlKey : Bool
    , keyCode : Int
    , metaKey : Bool
    , repeat : Bool
    , shiftKey : Bool
    }

keyboarEvent_toString : KeyboardEvent -> String
keyboarEvent_toString e =
    String.concat
        [ if e.ctrlKey then "C-" else ""
        , if e.altKey then "A-" else ""
        , if e.metaKey then "M-" else ""
        , if e.shiftKey then "S-"else ""
        , toString e.keyCode
        ]

decodeKeyboardEvent : Json.Decoder KeyboardEvent
decodeKeyboardEvent =
    Json.map6 KeyboardEvent
        (Json.field "altKey" Json.bool)
        (Json.field "ctrlKey" Json.bool)
        (Json.field "keyCode" Json.int)
        (Json.field "metaKey" Json.bool)
        (Json.field "repeat" Json.bool)
        (Json.field "shiftKey" Json.bool)    

                
onKeyDown : (KeyboardEvent -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger decodeKeyboardEvent)

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (Json.map tagger keyCode)

onKeyUp: (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


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

-- Focus Event

onFocusIn : (Bool -> msg) -> Attribute msg
onFocusIn tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusin" (Json.map tagger (Json.field "bubbles" Json.bool))

onFocusOut : (Bool -> msg) -> Attribute msg
onFocusOut tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusout" (Json.map tagger (Json.field "bubbles" Json.bool))

-- Mouse Event

onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Json.map tagger Mouse.position)

------------------------------------------------------------
-- Native (Mice)
------------------------------------------------------------

doFocus : String -> Bool
doFocus id = Native.Mice.doFocus id

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

setPreventDefaultKeyShortcut : String -> Bool
setPreventDefaultKeyShortcut id = Native.Mice.setPreventDefaultKeyShortcut id
