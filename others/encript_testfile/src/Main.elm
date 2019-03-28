module Main exposing (Model, Msg(..), init, main, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { userName : String
    , ssh_key : String
    }

init : Int -> ( Model, Cmd Msg )
init _ =
    (    Model "" ""
    , Cmd.none
    )

type Msg
    = InputUserName String
    | GetSshKey
    | GotSshkey (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUserName name ->
            ( { model| userName = name}
            , Cmd.none
            )

        GetSshKey ->
            ( model
            , getSshKey model.userName
            )

        GotSshkey (Ok sshkey) ->
            ( {model | ssh_key = sshkey}
            , Cmd.none
            )

        GotSshkey _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div []
        [ div []
              [ label [ for "account_id" ] [ text "Account ID: " ]
              , input
                    [ type_ "text"
                    , name "account_id"
                    , value model.userName
                    , onInput InputUserName
                    ] []
              ]
        , button [ disabled (model.userName == "")
                 , onClick GetSshKey
                 ] [text <| "Get SSH KEYs"]
        , div []
            [ text <| model.ssh_key ]
        ]

getSshKey : String -> Cmd Msg
getSshKey username =
    let
        url =
            "https://github.com/" ++ username ++ ".keys"
    in
    Http.get
        { url = url
        , expect = Http.expectString GotSshkey
        }
