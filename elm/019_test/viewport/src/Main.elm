module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task exposing (Task)


type alias Model =
    { cursor : Maybe Dom.Element
    }

type Msg 
    = Nop
    | GetCursorPos
    | UpdateCursorPos Dom.Element


main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

init : Maybe Int -> ( Model, Cmd Msg )
init flgs =
    ( Model Nothing
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        GetCursorPos ->
            ( model,
                  Dom.getElement "cursor"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateCursorPos domelm
                                           Err e     -> Nop
                                  )
            )
        UpdateCursorPos pos ->
            ( { model | cursor = Just pos}
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Browser.Dom の viewポート関連のデモンストレーション" ]
        , div [ onClick GetCursorPos ] [text "Get cursor position"]
        , viewElement model.cursor
        , div [ id "frame"
              , style "overflow" "auto"
              , style "height" "20rem"

              , style "border" "1px solid red"
              , style "background-color" "pink"
              ]
              [ div [ id "canvas"
                    , style "border" "1px solid green"
                    , style "font-size" "3rem"
                    ]
                    [ p [] [text "abcdefg"]
                    , p [] [text "hijklmn"]
                    , p [] [text "opqrstu"]
                    , p [] [text "vwxyz"]
                    , p [] [text "0123456789"]
                    , p [] [text "ABCDEFG"]
                    , p [] [text "HIJKLMN"]
                    , p [] [text "OPQRSTU"]
                    , p [] [text "VWXYZ"]
                    , p [] [text "あいうえお かきくけこ"]
                    , p [] [text "さしすせそ たちつてと"]
                    , p [] [text "なにぬねの はひふへほ"]
                    , p [] [text "まみむめも や　ゆ　よ"]
                    , p [] [text "らりるれろ わゐ　ゑを"]
                    , p [id "cursor"] [text "ん"]
                    ]
              ]
        ]


viewElement : Maybe Dom.Element -> Html msg
viewElement maybe_pos =
    case maybe_pos of
        Nothing ->
            div [] []
        Just pos ->
            ul [] [ li [] [ text "scene:"
                          , ul [] [ li [] [ text "width: ", text (String.fromFloat pos.scene.width) ]
                                  , li [] [ text "height: ", text (String.fromFloat pos.scene.height) ]
                                  ]
                          ]

                  , li [] [ text "viewport:"
                          , ul [] [ li [] [ text "x: ", text (String.fromFloat pos.viewport.x) ]
                                  , li [] [ text "y: ", text (String.fromFloat pos.viewport.y) ]
                                  , li [] [ text "width: ", text (String.fromFloat pos.viewport.width) ]
                                  , li [] [ text "height: ", text (String.fromFloat pos.viewport.height) ]
                                  ]
                          ]

                  , li [] [ text "element:"
                          , ul [] [ li [] [ text "x: ", text (String.fromFloat pos.element.x) ]
                                  , li [] [ text "y: ", text (String.fromFloat pos.element.y) ]
                                  , li [] [ text "width: ", text (String.fromFloat  pos.element.width) ]
                                  , li [] [ text "height: ", text (String.fromFloat pos.element.height) ]
                                  ]
                          ]
                  ]


