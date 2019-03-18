module Main exposing (Model, Msg(..), init, main, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init =
    Model "" Nothing


type alias Model =
    { source : String
    , message : Maybe String
    }


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input s ->
            { model | source = s }


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput Input, value model.source ] []
        , div [] [ model.message |> Maybe.withDefault "" |> text ]
        ]
