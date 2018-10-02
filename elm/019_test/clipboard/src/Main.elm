module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Decode

main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias Model =
    { text : String
    , message : String
    }

init : Maybe Int -> ( Model, Cmd Msg )
init flgs =
    ( Model "" ""
    , Cmd.none
    )



type Msg
    = UpdateText String
    | Paste String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText str ->
            ( { model | text = str }
            , Cmd.none
            )
        Paste str ->
            ( { model
                  | text = str
                  , message = "pasted \"" ++ str ++ "\""}
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ "Clipboard" |> text ]
        , h2 [] [ "buffer" |> text ]
        , div [] [ div [] [ model.text |> text ]
                 , textarea [ onInput UpdateText
                            , value model.text
                            ] []
                 ]
        , h2 [] [ "target" |> text ]
        , textarea [ onPaste Paste ] []

        , p [] [ "msg: " |> text
               , model.message |> text
               ]
        ]


onPaste : (String -> msg) -> Attribute msg
onPaste tagger =
  on "paste" (Decode.map
                  tagger
                  (Decode.succeed "hoge")
--                  (Decode.field "clipboarddata" Decode.string)
--                  (Decode.at ["clipboardData", "text/plane"]  Decode.string)
             )

              
