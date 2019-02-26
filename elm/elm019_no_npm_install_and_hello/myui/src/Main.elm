module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


type alias Model =
    { auth_id : String
    , auth_passwd : String
    }


type Msg
    = AuthId String
    | AuthPasswd String
    | Authentication


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthId id ->
            ( { model | auth_id = id }, Cmd.none )

        AuthPasswd passwd ->
            ( { model | auth_passwd = passwd }, Cmd.none )

        Authentication ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , value model.auth_id
            , onInput AuthId
            ]
            []
        , input
            [ type_ "password"
            , value model.auth_passwd
            , onInput AuthPasswd
            ]
            []
        , button [ onClick Authentication ] [ text "Authentication" ]
        ]
