module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task exposing (Task)

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
    }

type Msg 
    = Nop
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
    ( Model nullInfo nullInfo nullInfo
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
            ( { model | cursor = setPos (Just pos) model.cursor }
            , Cmd.none
            )

        GetCursorViewport ->
            ( model,
                  Dom.getViewportOf "cursor"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateCursorViewport domelm
                                           Err e     -> Nop
                                  )
            )

        UpdateCursorViewport vp ->
            ( { model | cursor = setVp (Just vp) model.cursor }
            , Cmd.none
            )

        GetFramePos ->
            ( model,
                  Dom.getElement "frame"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateFramePos domelm
                                           Err e     -> Nop
                                  )
            )
        UpdateFramePos pos ->
            ( { model | frame = setPos (Just pos) model.frame }
            , Cmd.none
            )


        GetFrameViewport ->
            ( model,
                  Dom.getViewportOf "frame"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateFrameViewport domelm
                                           Err e     -> Nop
                                  )
            )
        UpdateFrameViewport vp ->
            ( { model | frame = setVp (Just vp) model.frame }
            , Cmd.none
            )

        GetCanvasPos ->
            ( model,
                  Dom.getElement "canvas"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateCanvasPos domelm
                                           Err e     -> Nop
                                  )
            )
        UpdateCanvasPos pos ->
            ( { model | canvas = setPos (Just pos) model.canvas }
            , Cmd.none
            )


        GetCanvasViewport ->
            ( model,
                  Dom.getViewportOf "canvas"
                  |> Task.attempt (\r ->
                                       case r of
                                           Ok domelm -> UpdateCanvasViewport domelm
                                           Err e     -> Nop
                                  )
            )
        UpdateCanvasViewport vp ->
            ( { model | canvas = setVp (Just vp) model.canvas }
            , Cmd.none
            )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Browser.Dom の viewポート関連のデモンストレーション" ]
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

