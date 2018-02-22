
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
    , raw : String
    , raw_buf : String
    , hist : String
    }

type Msg
    = RawInput String
    | MoveForward
    | MoveBackword
    | MovePrevios
    | MoveNext
    | Backspace
    | Insert String
    | KeyEvent Int
    

init : (Model, Cmd Msg)
init =
    ( Model (Editor.init "") "" "" ""
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RawInput txt ->
            ({ model
                 | editor = Editor.init txt
                 , raw = txt}
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
                  , raw_buf = ""
              }
            , Cmd.none)

        Insert s ->
            ( { model
                  | editor = Editor.insert model.editor (model.editor.cursor.row, model.editor.cursor.column) (String.right 1 s)
                  , raw_buf = ""
                  , hist = ("(" ++ (String.right 1 s) ++ ") ") ++ model.hist
              }
            , Cmd.none)

        KeyEvent code ->
            keyInput code model


keyInput : Int -> Model -> (Model, Cmd Msg)
keyInput code model =
    case code of
        37 -> -- '←'
            ( { model
                  | editor = Editor.moveBackward model.editor
              }
            , Cmd.none)
        38 -> -- '↑'
            ( { model
                  | editor = Editor.movePrevios model.editor
              }
            , Cmd.none)
        39 -> -- '→'
            ( { model
                  | editor = Editor.moveForward model.editor
              }
            , Cmd.none)
        40 -> -- '↓'
            ( { model
                  | editor = Editor.moveNext model.editor
              }
            , Cmd.none)
        8 -> -- bs
            ( { model
                  | editor = Editor.backspace model.editor (model.editor.cursor.row, model.editor.cursor.column)
                  , raw_buf = ""
              }
            , Cmd.none)
        _ ->
            ( model, Cmd.none)


--        13 -> -- Enter
--            (newLine model, Cmd.none)







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
              ] [ text <| "(" ++ (toString model.editor.cursor.row) ++ ", " ++ (toString model.editor.cursor.column) ++ ")"]
        , div [] [ textarea [ onInput Insert
                            , onKeyDown KeyEvent
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

