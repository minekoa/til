module Editor.Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Time exposing (Time, second)


import Editor.Buffer as Buffer
import Native.Mice

{-| This module is simple texteditor.

-}

type alias Model =
    { id : String -- frame id

    , buffer : Buffer.Model

    -- frame
    , input_buffer : String
    , enableComposer : Bool
    , compositionData : Maybe String --IMEで返還中の未確定文字
    , focus : Bool
    , blink : BlinkState

    -- for debug
    , event_memo : List String
    }

init : String -> String -> Model
init id text =
    Model id                     -- id
          (Buffer.init text)
          ""                     -- input_buffer
          False Nothing          -- COMPOSER STATE
          False                  -- focus
          BlinkBlocked           -- blink

          []                     -- event_memo



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
            ( {model| focus = True}
            , Cmd.none)
        FocusOut _ ->
            ( {model|focus = False}
            , Cmd.none)
        SetFocus ->
            ( {model | focus = doFocus (model.id ++ "-input")}
            , Cmd.none )

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )



keyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
keyDown e model =
    case e.keyCode of
        37 -> -- '←'
            ( moveBackward model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none )
        38 -> -- '↑'
            ( movePrevios model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        39 -> -- '→'
            ( moveForward model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        40 -> -- '↓'
            ( moveNext model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        8 -> -- bs
            ( backspace model (Buffer.nowCursorPos model.buffer)
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        46 -> -- del
            ( delete model (Buffer.nowCursorPos model.buffer)
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        90 -> -- 'Z'
            -- note: ためしに Ctrl-Z で undo を実装したが、
            --       textareaの持つ undo機能が発動し、
            --       （空文字の）input がされてしまう不具合を発見.
            --       結果、まだ「使えない」実装に。
            --       どう対処したものか...
            if (e.ctrlKey && not e.altKey  && not e.metaKey && not e.shiftKey)
            then
                ( undo model
                  |> eventMemorize ("D:*" ++ keyboarEvent_toString e)
                , Cmd.none)
            else
                ( model
                  |> eventMemorize ("D:"++ keyboarEvent_toString e)
                , Cmd.none)
        _ ->
            ( model
              |> eventMemorize ("D:"++ keyboarEvent_toString e)
            , Cmd.none)

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
      |> inputBufferClear
      |> blinkBlock
      |> eventMemorize ("Cs{" ++ data ++ "} ")
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
      |> inputBufferClear
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
    |> blinkBlock

moveBackward : Model -> Model
moveBackward model =
    { model | buffer = Buffer.moveBackward model.buffer }
    |> blinkBlock

movePrevios : Model -> Model
movePrevios model =
    { model | buffer = Buffer.movePrevios model.buffer }
    |> blinkBlock

moveNext : Model -> Model
moveNext model =
    { model | buffer = Buffer.moveNext model.buffer }
    |> blinkBlock


------------------------------------------------------------
-- edit
------------------------------------------------------------

insert: Model -> (Int, Int)  -> String -> Model
insert model (row, col) text =
    { model | buffer = Buffer.insert (row, col) text model.buffer }
    |> blinkBlock

backspace: Model -> (Int, Int) -> Model
backspace model (row, col) =
    { model | buffer = Buffer.backspace (row, col) model.buffer}
    |> blinkBlock

delete: Model -> (Int, Int) -> Model
delete model (row, col) =
    { model | buffer = Buffer.delete (row, col) model.buffer}
    |> blinkBlock

undo : Model -> Model
undo model =
    { model | buffer = Buffer.undo model.buffer }
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
        [ cursorLayer2 model
        , codeLayer model
        ]

codeLayer: Model  -> Html Msg
codeLayer model = 
    let
        contents = model.buffer.contents
        cursor = model.buffer.cursor
    in
        div [class "lines"] <|
            List.indexedMap
                (λ n ln ->
                      if n == cursor.row then
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
--                                      , ("display" , "inline-flex")
                                      ]
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
                              ] [text ln] ) contents

cursorLayer2 : Model -> Html Msg
cursorLayer2 model =
    {- 作戦2, [パディング用要素、本物の文字を入れ、hiddenにする][cursol]
       思いついたのでやってみたらうまく行った
       けど、みんなこれをやってないの、なにか落とし穴あるからなのかな？
    -}
    div [ class "cursors"
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
               [ ruler model
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


ruler : Model -> Html msg
ruler model = 
    let
        cur      = model.buffer.cursor
        contents = model.buffer.contents
    in
    span [ id "ruler"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ Buffer.line cur.row contents |> Maybe.withDefault "" |> String.left cur.column |> text ]

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


------------------------------------------------------------
-- Native (Mice)
------------------------------------------------------------

doFocus : String -> Bool
doFocus id = Native.Mice.doFocus id

