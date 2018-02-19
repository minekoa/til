
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
    }

type Msg
    = RawInput String
    | MoveForward
    | MoveBackword
    | MovePrevios
    | MoveNext
    

init : (Model, Cmd Msg)
init =
    ( Model (Editor.init "") ""
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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [text "TextEditor Sample"]
        , Editor.view model.editor
        , div [class "modeline"] [ text <| "(" ++ (toString model.editor.cursor.row) ++ ", " ++ (toString model.editor.cursor.column) ++ ")"]
        , textarea [onInput RawInput] [text model.raw]
        , div [] [ button [ onClick MoveBackword ] [text "←"]
                 , button [ onClick MovePrevios  ] [text "↑"]
                 , button [ onClick MoveNext     ] [text "↓"]
                 , button [ onClick MoveForward  ] [text "→"]
                 ]
        ]
