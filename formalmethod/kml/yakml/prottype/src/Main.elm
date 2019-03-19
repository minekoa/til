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


sampleCode =
    """state Foo @ (-The_state_of_foo-) {
    transition eventA --> {
        post {
          hogehoge
        } @  {-Post_Conditions_Comment-}
    }
    transition eventB
    --> {
        post {
           piyopiyo
        } @ {-AAA-}
    }
    transition eventB
          when guird_expression
            @[-Guird_clause_optional-]
    --> {
        post {
           fugafuga
        } @ {-CCC-}
    }
}
"""


init =
    Model
        sampleCode
        (KmlParser.parse sampleCode)
        Nothing


type alias Model =
    { source : String
    , parseResult : Result (List Parser.DeadEnd) KmlParser.State
    , message : Maybe String
    }


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input s ->
            { model
                | source = s
                , parseResult = KmlParser.parse s
            }


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput Input, value model.source ] []
        , div [] [ model.message |> Maybe.withDefault "" |> text ]
        , div []
            [ case model.parseResult of
                Ok s ->
                    text <| Debug.toString s

                Err e ->
                    text <| Debug.toString e
            ]
        , case model.parseResult of
            Ok s ->
                stateView s

            Err e ->
                text ""
        ]


stateView : KmlParser.State -> Html Msg
stateView state =
    let
        headerline =
            tr []
                [ th [] [ text "event" ]
                , th [] [ text "guird(expression)" ]
                , th [] [ text "guird(natulal)" ]
                , th [] [ text "post(expression)" ]
                , th [] [ text "post(natural)" ]
                ]

        transitionToLine =
            \trn ->
                tr []
                    [ td [] [ trn.event |> text ]
                    , td [] [ trn.guird |> Maybe.andThen (.expression  >> Just) |> Maybe.withDefault "" |> text ]
                    , td [] [ trn.guird |> Maybe.andThen (.naturalLang >> Just) |> Maybe.withDefault "" |> text ]
                    , td [] [ trn.post.expression |> text ]
                    , td [] [ trn.post.naturalLang |> text ]
                    ]
    in
    div []
        [ h1 [] [ text <| state.name ]
        , p [] [ text <| state.naturalLang ]
        , table [] <|
            headerline
                :: (state.body |> List.map transitionToLine)
        ]
