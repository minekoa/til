import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode

import EscapedTextInput.EscapedTextInput as TextInput


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias Model =
    { textinput : TextInput.Model }

type Msg =
    TextInputMsg TextInput.Msg



init : (Model, Cmd Msg)
init =
    ( Model
          (TextInput.init "")
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextInputMsg imsg ->
            let
                (im, ic) = TextInput.update imsg model.textinput
            in
                ( { model
                      | textinput = im
                  }
                , Cmd.map TextInputMsg ic
                ) 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ div [ style [ ("border", "1px solid gray")
                      , ("margin", "1rem")
                      , ("padding", "0.5em")
                      ]
              ]
              [ Html.map TextInputMsg (TextInput.view model.textinput) ]
        , h2 [] ["menbers" |> text]
        , table []
            [ tr [] [ th [] [ "text" |> text ]
                    , td [] [ model.textinput.text |> text ]
                    ]
            , tr [] [ th [] [ "pos" |> text ]
                    , td [] [ model.textinput.pos |> toString |> text ]
                    ]
            , tr [] [ th [] [ "focus" |> text ]
                    , td [] [ model.textinput.focus |> toString |> text ]
                    ]
            , tr [] [ th [] [ "compositonInput" |> text ]
                    , td [] [ model.textinput.compositionInput |> Maybe.withDefault "Nothing" |> text ]
                    ]
            , tr [] [ th [] [ "value" |> text ]
                    , td [] [ model.textinput.value |> Maybe.withDefault "Nothing" |> text ]
                    ]
            ]
        , div []
            (List.map (\elm -> div [] [text elm]) model.textinput.eventLog)
        ]



