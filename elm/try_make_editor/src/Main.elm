import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Json

import Editor.Editor as Editor

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias Model =
    { editor : Editor.Model
    , raw_buf : String
    , hist : String
    , enableIME : Bool
    , previosKeyEvent : KeyEventKind
    }

type KeyEventKind
    = KeyDownEvent
    | KeyPressEvent
    | KeyUpEvent

type Msg
    = RawInput String
    | MoveForward
    | MoveBackword
    | MovePrevios
    | MoveNext
    | Backspace
    | Insert String
    | KeyDown Int
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String


init : (Model, Cmd Msg)
init =
    ( Model (Editor.init "") "" "" False KeyUpEvent
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RawInput txt ->
            ( { model
                  | editor = Editor.init txt
              }
            , Cmd.none)
        MoveForward ->
            ( { model
                  | editor = Editor.moveForward model.editor
              }
            , Cmd.none)
        MoveBackword ->
            ( { model
                  | editor = Editor.moveBackward model.editor
              }
            , Cmd.none)
        MovePrevios ->
            ( { model
                  | editor = Editor.movePrevios model.editor
              }
            , Cmd.none)
        MoveNext ->
            ( { model
                  | editor = Editor.moveNext model.editor
              }
            , Cmd.none)

        Backspace ->
            ( { model
                  | editor = Editor.backspace model.editor (model.editor.cursor.row, model.editor.cursor.column)
              }
            , Cmd.none)

        Insert s ->
            case model.enableIME of
                True ->
                    let editor = model.editor in
                    ( { model
                          | raw_buf =  s
                          , hist = ("<<" ++ s ++ ">> ") ++ model.hist
                      }
                    , Cmd.none )
                False ->
                    ( { model
                          | editor = Editor.insert model.editor (model.editor.cursor.row, model.editor.cursor.column) (String.right 1 s)
                          , raw_buf =  ""
                          , hist = ("(" ++ (String.right 1 s) ++ ") ") ++ model.hist
                      }
                    , Cmd.none)

        KeyDown code ->
            keyDown code model

        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            compositionEnd data model


keyDown : Int -> Model -> (Model, Cmd Msg)
keyDown code model =
    case code of
        37 -> -- '←'
            ( { model
                  | editor = Editor.moveBackward model.editor
                  , previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)
        38 -> -- '↑'
            ( { model
                  | editor = Editor.movePrevios model.editor
                  , previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)
        39 -> -- '→'
            ( { model
                  | editor = Editor.moveForward model.editor
                  , previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)
        40 -> -- '↓'
            ( { model
                  | editor = Editor.moveNext model.editor
                  , previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)
        8 -> -- bs
            ( { model
                  | editor = Editor.backspace model.editor (model.editor.cursor.row, model.editor.cursor.column)
                  , enableIME = False
                  , previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)

        _ ->
            ( { model
                  | previosKeyEvent = KeyDownEvent
                  , hist = "D " ++ model.hist
              }
            , Cmd.none)


keyPress : Int -> Model -> (Model, Cmd Msg)
keyPress code model =
    case code of
        _ ->
            ( { model
                  | previosKeyEvent = KeyPressEvent
                  , hist = "P " ++ model.hist
              }
            , Cmd.none)

keyUp : Int -> Model -> (Model, Cmd Msg)
keyUp code model =
    case code of
        13 -> -- Enter
            if model.previosKeyEvent == KeyDownEvent then
                let
                    e1 = model.editor
                    e2 = {e1 | compositionData = Nothing}
                in
                ( { model
                      | editor = Editor.insert e2 (e2.cursor.row, e2.cursor.column) (Maybe.withDefault "" e1.compositionData)
                      , raw_buf = ""
                      , previosKeyEvent = KeyUpEvent
                      , hist = "U " ++ model.hist
                      , enableIME = False
                  }
                , Cmd.none )
            else 
                ( { model
                      | previosKeyEvent = KeyUpEvent
                      , hist = "U " ++ model.hist
                  }
                , Cmd.none)
        _ ->
            ( { model
                  | previosKeyEvent = KeyUpEvent
                  , hist = "U " ++ model.hist
              }
            , Cmd.none)

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    let
        e1 = model.editor
        e2 = {e1 | compositionData = Just(data)}
    in
    ( { model
          | enableIME = True
          , raw_buf = ""
          , editor = e2
          , hist = "Cs{" ++ data ++ "} " ++ model.hist
      }
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    let
        e1 = model.editor
        e2 = {e1 | compositionData = Just(data)}
    in
    ( { model
          | hist = "Cu{" ++ data ++ "} " ++ model.hist
          , editor = e2
      }
    , Cmd.none
    )


compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    let
        e1 = model.editor
        e2 = {e1 | compositionData = Nothing}
    in
    ( { model
          | editor = Editor.insert e2 (e2.cursor.row, e2.cursor.column) data
          , raw_buf = ""
          , hist = "Ce{" ++ data ++ "} " ++ model.hist
          , enableIME = False
      }
    , Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("display", "flex"), ("flex-direction", "column")
                ]
        ]
        [ h1 [] [text "TextEditor Sample"]
        , div [ style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                      , ("flex-grow", "8")
                      , ("overflow","auto")
                      ]
              ] [ Editor.view model.editor ]
        , div [ class "modeline"
              , style [ ("background-color","black")
                      , ("color", "white")
                      ]
              ] [ text <| "(" ++ (toString model.editor.cursor.row) ++ ", " ++ (toString model.editor.cursor.column) ++ ")"
                , text <| if model.enableIME then ("[IME] " ++ (Maybe.withDefault "" model.editor.compositionData) ) else "" 
                ]
        , div [] [ textarea [ onInput Insert
                            , onKeyDown KeyDown
                            , onCompositionStart CompositionStart
                            , onCompositionUpdate CompositionUpdate
                            , onCompositionEnd CompositionEnd
                            , value model.raw_buf] []
                 , button [ onClick Backspace ] [ text "Backspace" ]
                 ]
        , div [] [ button [ onClick MoveBackword ] [text "←"]
                 , button [ onClick MovePrevios  ] [text "↑"]
                 , button [ onClick MoveNext     ] [text "↓"]
                 , button [ onClick MoveForward  ] [text "→"]
                 ]
        , div [ style [ ("overflow","scroll")
                      , ("width", "calc( 100% - 2px )")
                      , ("border", "1px solid black")
                      , ("flex-grow", "3")
                      , ("min-height", "3em")
                      ]
              ] [ text model.hist ]
        ]

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (Json.map tagger keyCode)

onKeyUp: (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)

onCompositionStart: (String -> msg) -> Attribute msg
onCompositionStart tagger =
    on "compositionstart" (Json.map tagger (Json.field "data" Json.string))

onCompositionEnd: (String -> msg) -> Attribute msg
onCompositionEnd tagger =
    on "compositionend" (Json.map tagger (Json.field "data" Json.string))

onCompositionUpdate: (String -> msg) -> Attribute msg
onCompositionUpdate tagger =
    on "compositionupdate" (Json.map tagger (Json.field "data" Json.string))


