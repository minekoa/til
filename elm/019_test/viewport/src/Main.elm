module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task exposing (Task)
import Time exposing (Posix, toMillis)

type alias DomInfo =
    { pos : Maybe Dom.Element
    , vp  : Maybe Dom.Viewport
    }

setPos : Maybe Dom.Element -> DomInfo -> DomInfo
setPos pos info =
    { info | pos = pos }

setVp : Maybe Dom.Viewport -> DomInfo -> DomInfo
setVp vp info =
    { info | vp = vp }

nullInfo : DomInfo
nullInfo = { pos = Nothing, vp = Nothing }

type alias Model =
    { cursor : DomInfo
    , frame  : DomInfo
    , canvas : DomInfo
    , isAutoUpdate : Bool
    }

type Msg 
    = Nop
    | Tick Posix
    | SetAutoUpdate Bool
    | GetCursorPos
    | UpdateCursorPos Dom.Element
    | GetCursorViewport
    | UpdateCursorViewport Dom.Viewport
    | GetFramePos
    | UpdateFramePos Dom.Element
    | GetFrameViewport
    | UpdateFrameViewport Dom.Viewport
    | GetCanvasPos
    | UpdateCanvasPos Dom.Element
    | GetCanvasViewport
    | UpdateCanvasViewport Dom.Viewport

main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

init : Maybe Int -> ( Model, Cmd Msg )
init flgs =
    ( Model nullInfo nullInfo nullInfo False
    , Cmd.none
    )

getElementCmd: String -> (Dom.Element -> Msg) -> Cmd Msg
getElementCmd id updtMsg =
    Dom.getElement id
        |> Task.attempt (\r ->
                             case r of
                                 Ok domelm -> updtMsg domelm
                                 Err e     -> Nop
                        )

getViewportCmd: String -> (Dom.Viewport -> Msg) -> Cmd Msg
getViewportCmd id updtMsg =
    Dom.getViewportOf id
        |> Task.attempt (\r ->
                             case r of
                                 Ok domelm -> updtMsg domelm
                                 Err e     -> Nop
                        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Tick tm ->
            ( model
            , case model.isAutoUpdate of
                  False ->
                      Cmd.none
                  True ->
                      Cmd.batch [ getElementCmd "cursor" UpdateCursorPos
                                , getViewportCmd "cursor" UpdateCursorViewport
                                , getElementCmd "frame" UpdateFramePos
                                , getViewportCmd "frame" UpdateFrameViewport
                                , getElementCmd "canvas" UpdateCanvasPos
                                , getViewportCmd "canvas" UpdateCanvasViewport
                                ]
            )

        SetAutoUpdate b ->
            ( { model | isAutoUpdate = b }
            , Cmd.none
            )

        GetCursorPos ->
            ( model
            , getElementCmd "cursor" UpdateCursorPos
            )
        UpdateCursorPos pos ->
            ( { model | cursor = setPos (Just pos) model.cursor }
            , Cmd.none
            )

        GetCursorViewport ->
            ( model
            , getViewportCmd "cursor" UpdateCursorViewport
            )

        UpdateCursorViewport vp ->
            ( { model | cursor = setVp (Just vp) model.cursor }
            , Cmd.none
            )

        GetFramePos ->
            ( model
            , getElementCmd "frame" UpdateFramePos
            )
        UpdateFramePos pos ->
            ( { model | frame = setPos (Just pos) model.frame }
            , Cmd.none
            )
        GetFrameViewport ->
            ( model
            , getViewportCmd "frame" UpdateFrameViewport
            )
        UpdateFrameViewport vp ->
            ( { model | frame = setVp (Just vp) model.frame }
            , Cmd.none
            )

        GetCanvasPos ->
            ( model
            , getElementCmd "canvas" UpdateCanvasPos
            )
        UpdateCanvasPos pos ->
            ( { model | canvas = setPos (Just pos) model.canvas }
            , Cmd.none
            )
        GetCanvasViewport ->
            ( model
            , getViewportCmd "canvas" UpdateCanvasViewport
            )
        UpdateCanvasViewport vp ->
            ( { model | canvas = setVp (Just vp) model.canvas }
            , Cmd.none
            )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 Tick ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Browser.Dom の viewポート関連のデモンストレーション" ]
        , div [ onClick <| SetAutoUpdate (not model.isAutoUpdate)
              , style "color" <| case model.isAutoUpdate of
                                     True -> "red"
                                     False -> "gray"
              ]
              [ text "Auto Update" ]
        , div [style "display" "flex" ]
            [ div [] [ div [ class "btn", onClick GetCursorPos ] [text "getElement 'cursor'"]
                     , viewElement model.cursor.pos
                     ]
            , div [] [ div [ class "btn", onClick GetCursorViewport ] [text "getViewportOf 'cursor'"]
                     , viewViewport model.cursor.vp
                     ]
            , div [] [ div [ class "btn", onClick GetFramePos ] [text "getElement 'frame'"]
                     , viewElement model.frame.pos
                     ]
            , div [] [ div [ class "btn", onClick GetFrameViewport ] [text "getViewportOf 'frame'"]
                     , viewViewport model.frame.vp
                     ]
            , div [] [ div [ class "btn", onClick GetCanvasPos ] [text "getElement 'canvas'"]
                     , viewElement model.canvas.pos
                     ]
            , div [] [ div [ class "btn", onClick GetCanvasViewport ] [text "getViewportOf 'canvas'"]
                     , viewViewport model.canvas.vp
                     ]
            ]
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

viewViewport : Maybe Dom.Viewport -> Html msg
viewViewport maybe_vp =
    case maybe_vp of
        Nothing ->
            div [] []
        Just vp ->
            ul [] [ li [] [ text "scene:"
                          , ul [] [ li [] [ text "width: ", text (String.fromFloat vp.scene.width) ]
                                  , li [] [ text "height: ", text (String.fromFloat vp.scene.height) ]
                                  ]
                          ]

                  , li [] [ text "viewport:"
                          , ul [] [ li [] [ text "x: ", text (String.fromFloat vp.viewport.x) ]
                                  , li [] [ text "y: ", text (String.fromFloat vp.viewport.y) ]
                                  , li [] [ text "width: ", text (String.fromFloat vp.viewport.width) ]
                                  , li [] [ text "height: ", text (String.fromFloat vp.viewport.height) ]
                                  ]
                          ]
                  ]

