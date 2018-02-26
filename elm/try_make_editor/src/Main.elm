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
    }

type Msg
    = RawInput String
    | MoveForward
    | MoveBackword
    | MovePrevios
    | MoveNext
    | Backspace
    | EditorMsg (Editor.Msg)

init : (Model, Cmd Msg)
init =
    ( Model (Editor.init "") ""
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

        -- ScenarioPage >> List
        EditorMsg msg ->
            let
                (m, c) = Editor.update msg model.editor
            in
                ( { model | editor = m}
                , Cmd.map EditorMsg c )


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
              ] [ Html.map EditorMsg (Editor.view model.editor) ]
        , div [ class "modeline"
              , style [ ("background-color","black")
                      , ("color", "white")
                      ]
              ] [ text <| "(" ++ (toString model.editor.cursor.row) ++ ", " ++ (toString model.editor.cursor.column) ++ ")"
                , text <| if model.editor.enableComposer then ("[IME] " ++ (Maybe.withDefault "" model.editor.compositionData) ) else "" 
                ]
        , div [] [ button [ onClick MoveBackword ] [text "←"]
                 , button [ onClick MovePrevios  ] [text "↑"]
                 , button [ onClick MoveNext     ] [text "↓"]
                 , button [ onClick MoveForward  ] [text "→"]
                 , text "|"
                 , button [ onClick Backspace ] [ text "Backspace" ]
                 ]
        , div [ style [ ("overflow","scroll")
                      , ("width", "calc( 100% - 2px )")
                      , ("border", "1px solid black")
                      , ("flex-grow", "3")
                      , ("min-height", "3em")
                      , ("max-height", "8em")
                      ]
              ] 
              ( List.map (λ ln -> span [ style [("margin-right","0.2em")]] [text ln]) model.editor.event_memo )
        ]

