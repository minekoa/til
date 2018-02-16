
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

type Msg =
    RawInput String


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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [text "TextEditor Sample"]
        , Editor.view model.editor
        , textarea [onInput RawInput] [text model.raw]
        ]
