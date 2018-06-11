module EscapedTextInput.EscapedTextInput exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Task
import Dom


import StringTools exposing (..)
import KeyEvent exposing (..)

type alias Model =
    { text : String
    , pos  : Int
    , focus : Bool
    , compositionInput : Maybe String
    , value : Maybe String
    , eventLog : List String
    }

init s =
    { text = s
    , pos  = 0
    , focus = False
    , compositionInput = Nothing
    , value = Just ""
    , eventLog = []
    }

type Msg
   = Input String Bool
   | CompositionStart String
   | CompositionUpdate String
   | CompositionEnd String
   | KeyDown KeyboardEvent
   | KeyUp Int
   | Focus Bool
   | Click


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input s isComposing ->
            ( if isComposing then
                  { model
                      | eventLog = ("input(isComposing) \"" ++ s ++ "\"") :: model.eventLog
                  }
              else
                  model
                      |> insert s
                      |> \m -> { m
                                 | compositionInput = Nothing
                                 , eventLog = ("input \"" ++ s ++ "\"") :: model.eventLog
                               }
            , Cmd.none
            )

        CompositionStart s ->
            ( { model | eventLog = "compositionStart" :: model.eventLog }
            , Cmd.none
            )

        CompositionUpdate s ->
            ( { model | eventLog = ("compositionUpdate \"" ++ s ++ "\"") :: model.eventLog 
                      , compositionInput = Just s
                      , value = Just s
              }
            , Cmd.none
            )

        CompositionEnd s ->
            -- chrome は、compositionEnd で入力確定しないと他にタイミングがない
            -- しかし、firefox は、compositionEnd の後に input isComposing=true で確定入力イベントが発火するので
            -- この作りでは2重入力になってしまう
            ( model
                |> insert s
                |> \m -> { m
                           | compositionInput = Nothing
                           , eventLog = ("compositionEnd \"" ++ s ++ "\"") :: model.eventLog
                         }
            , Cmd.none
            )

        KeyDown e ->
           ( model
                |> (case e.keyCode of
                        9  -> insert "\t"
                        37 -> moveBackword
                        39 -> moveForeword
                        8  -> backspace
                        46 -> delete
                        13 -> insert "\n"
                        _  -> identity
                   )
--                |> \m -> { m | eventLog = ("keydown " ++ (toString e))  :: model.eventLog }
           , Debug.log "keydown" Cmd.none
           )

        KeyUp e ->
            (  model
--                |> \m -> { m | eventLog = ("keyup " ++ (toString e))  :: model.eventLog }
            , Cmd.none
            )

        Focus focus ->
            ( { model | focus = focus }
            , Cmd.none
            )

        Click ->
            ( model
            , doFocus Focus
            )

------------------------------------------------------------
-- editor commands
------------------------------------------------------------

moveForeword : Model -> Model
moveForeword model =
    { model | pos = model.pos + 1 |> Basics.min (String.length model.text) }

moveBackword : Model -> Model
moveBackword model =
    { model | pos = model.pos - 1 |> Basics.max 0 }

insert : String -> Model -> Model
insert s model =
    { model
        | text = (String.left model.pos model.text) ++ s ++ (String.dropLeft model.pos model.text)
        , pos  = model.pos + (String.length s)
        , value = Just ""
    }

delete : Model -> Model
delete model =
    { model
        | text = (String.left model.pos model.text) ++ (String.dropLeft (model.pos + 1) model.text)
        , value = Just ""
    }

backspace : Model -> Model
backspace model =
    { model
        | text = (String.left (model.pos - 1) model.text) ++ (String.dropLeft model.pos model.text)
        , pos  = model.pos - 1
        , value = Just ""
    }


------------------------------------------------------------
-- view
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ style [ ("position", "relative")
                , ("color", if model.focus then "black" else "lightgray")
                , ("height", "1.5rem")
                ]
        , onClick Click
        ]

        -- text
        [ div [ style [ ("position", "absolute")
                      , ("top", "0"), ("left", "0")
                      ]
              ]
              ( case model.compositionInput of
                    Just s ->
                        [ model.text |> String.left model.pos |> stringEscape |> text
                        , span [ style [ ("color", "blue") ] ] [ s |> text ]
                        , model.text |> String.dropLeft model.pos |> stringEscape |> text
                        ]

                    Nothing ->
                        [ model.text |> stringEscape |> text]
              )

        -- cursor
        , div [ style [ ("position", "absolute")
                      , ("top", "0"), ("left", "0")
                      , ("display", "inline-flex")
                      ]
              ]
              [ div [ style [("opacity", "0")] ]
                    [ model.text |> String.left model.pos |> stringEscape |> text ]

              , div [ style [("position", "relative")] ]
                    [ div [ style [("opacity", "0")] ]
                          [ model.compositionInput |> Maybe.withDefault "" |> stringEscape |> text ]

                    , div [ style [ ("position", "absolute")
                                  , ("top", "0"), ("left", "0")
                                  , ("max-width", "1px")
                                  , ("width", "1px")
                                  , ("height", "1em")
                                  ]
                          ]
                          [ textarea [ style [ ("height", "1em")
                                             , ("width", model.compositionInput |> Maybe.withDefault "" |> String.length |> flip (+) 1 |> toString |> flip (++) "em" )
                                             , ("overflow", "hidden")
                                             , ("resize", "none")
                                             , ("opacity", "0")
                                             ]
                                     , id "keyboard_controller"
                                     , spellcheck False
                                     , wrap "off"
                                     , onCompositionStart CompositionStart
                                     , onCompositionUpdate CompositionUpdate
                                     , onCompositionEnd CompositionEnd
                                     , onKeyDown KeyDown
                                     , onKeyUp KeyUp
                                     , onInputEx Input
                                     , onFocusIn Focus
                                     , onFocusOut Focus
                                     , model.value |> Maybe.withDefault "" |> value -- どうもCompositionEnd がchromeで発火しない原因になっているようだ
                                     ]
                                     []
                          ]
                    ]

              , div [ style [ ("background-color",  if model.focus then "black" else "lightgray")
                            , ("width", "2px")
                            ]
                    ]
                    []
              , div [ style [ ("opacity", "0") ] ] [ text "D" ] -- 完全にこのレイヤー内の文字が空になってしまうと、なぜかこの要素がレンダリングされなくなってしまうため、ダミーで"D" を出力
              ]
        ]

------------------------------------------------------------
-- html event
------------------------------------------------------------

doFocus: (Bool -> msg) -> Cmd msg
doFocus tagger =
    Task.attempt (\_ -> tagger True) (Dom.focus "keyboard_controller")

onFocusIn : (Bool -> msg) -> Attribute msg
onFocusIn tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusin" (Json.Decode.map (\dmy -> tagger True) (Json.Decode.field "bubbles" Json.Decode.bool))

onFocusOut : (Bool -> msg) -> Attribute msg
onFocusOut tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusout" (Json.Decode.map (\dmy -> tagger False) (Json.Decode.field "bubbles" Json.Decode.bool))


onKeyDown : (KeyboardEvent -> msg) -> Attribute msg
onKeyDown tagger = 
    onWithOptions "keydown" { stopPropagation = True, preventDefault = True } <|
        considerKeyboardEvent (\ kbd_ev ->
                                   case kbd_ev.keyCode of
                                       9  -> Just (tagger kbd_ev) -- tab
                                       37 -> Just (tagger kbd_ev) -- "←"
                                       39 -> Just (tagger kbd_ev) -- "→"
                                       8  -> Just (tagger kbd_ev) -- "BackSpace"
                                       46 -> Just (tagger kbd_ev) -- "Delete"
                                       13 -> Just (tagger kbd_ev) -- "\n"
                                       _  -> Nothing
                              )

onKeyUp: (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger keyCode)

onInputEx : (String -> Bool -> msg) -> Attribute msg
onInputEx tagger =
    -- note: chromeにて、改行文字の入力を拾えないため、 onInput の上位互換にはならない (dataプロパティの振る舞いのようだ。おそらくだが）
    on "input" <|
        Json.Decode.map2 tagger
            (Json.Decode.field "data" Json.Decode.string)
            (Json.Decode.field "isComposing" Json.Decode.bool)

onCompositionStart: (String -> msg) -> Attribute msg
onCompositionStart tagger =
    on "compositionstart" (Json.Decode.map tagger (Json.Decode.field "data" Json.Decode.string))

onCompositionUpdate: (String -> msg) -> Attribute msg
onCompositionUpdate tagger =
    on "compositionupdate" (Json.Decode.map tagger (Json.Decode.field "data" Json.Decode.string))

onCompositionEnd: (String -> msg) -> Attribute msg
onCompositionEnd tagger =
    on "compositionend" (Json.Decode.map tagger (Json.Decode.field "data" Json.Decode.string))



