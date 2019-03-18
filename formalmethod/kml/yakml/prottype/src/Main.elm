module Main exposing (Model, Msg(..), init, main, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import KmlParser
import Parser

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init =
    Model "" Nothing Nothing


type alias Model =
    { source : String
    , parseResult : Maybe (Result (List Parser.DeadEnd) KmlParser.State)
    , message : Maybe String
    }


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input s ->
            { model |
                  source = s
            ,  parseResult = Just (KmlParser.parse s)
            }


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput Input, value model.source ] []
        , div [] [ model.message |> Maybe.withDefault "" |> text ]
        , div [] [ case model.parseResult of
                       Nothing -> text ""
                       Just ret ->
                           case ret of
                               Ok s ->
                                   text <| Debug.toString s
                               Err e ->
                                   text <| Debug.toString e
                 ]
        ]
