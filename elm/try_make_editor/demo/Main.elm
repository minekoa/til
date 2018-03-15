import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Json

import Editor
import Editor.Buffer as Buffer

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias Model =
    { editor : Editor.Model
    , raw_buf : String
    }

type Msg
    = MoveForward
    | MoveBackword
    | MovePrevios
    | MoveNext
    | Backspace
    | Delete
    | Copy
    | Cut
    | Pasete
    | Undo
    | EditorMsg (Editor.Msg)
    | SetEventlogEnable Bool

init : (Model, Cmd Msg)
init =
    let
        (bm, bc) = Editor.init "editor-sample1" ""
    in
        ( Model bm ""
        , Cmd.map EditorMsg bc
        )

updateMap: Model -> (Editor.Model, Cmd Editor.Msg) -> (Model, Cmd Msg)
updateMap model (em, ec) =
    ( {model | editor = em}
    , Cmd.map EditorMsg ec)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MoveForward ->
            updateMap model (Editor.moveForward model.editor)

        MoveBackword ->
            updateMap model (Editor.moveBackward model.editor)

        MovePrevios ->
            updateMap model (Editor.movePrevios model.editor)

        MoveNext ->
            updateMap model (Editor.moveNext model.editor)

        Backspace ->
            updateMap model (Editor.backspace model.editor)

        Delete ->
            updateMap model (Editor.delete model.editor)

        Copy ->
            updateMap model (Editor.copy model.editor)

        Cut ->
            updateMap model (Editor.cut model.editor)

        Pasete ->
            updateMap model (Editor.paste model.editor.copyStore model.editor)

        Undo ->
            updateMap model (Editor.undo model.editor)

        SetEventlogEnable True ->
            let
                em = model.editor
            in
                ( { model
                      | editor = {em| event_log = Just []}
                  }
                , Cmd.none)

        SetEventlogEnable False ->
            let
                em = model.editor
            in
                ( { model
                      | editor = {em| event_log = Nothing}
                  }
                , Cmd.none)

        -- ScenarioPage >> List
        EditorMsg msg ->
            let
                (m, c) = Editor.update msg model.editor
            in
                ( { model | editor = m}
                , Cmd.map EditorMsg c )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map EditorMsg  (Editor.subscriptions model.editor)
              ]


view : Model -> Html Msg
view model =
    div [ style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("display", "flex"), ("flex-direction", "column")
                ]
        ]
        [ h1 [] [text "TextEditor Sample"]
        , div [ style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                      , ("overflow","hidden")
                      , ("flex-grow", "8")
                      ]
              ] [ Html.map EditorMsg (Editor.view model.editor) ]
        , modeline model
        , controlPane model
        , debugPane model
        ]

modeline : Model -> Html msg
modeline model =
    let
        toCursorString    = \c -> "(" ++ (toString c.row) ++ ", " ++ (toString c.column) ++ ")"
        toIMEString       =
            \ compositionData -> compositionData
                          |> Maybe.andThen (\d -> Just <| "[IME] " ++ d )
                          |> Maybe.withDefault ""
        toSelectionString =
            \ selection -> selection
                        |> Maybe.andThen (\s-> Just <|
                                              " select:(" ++ (s.begin |> Tuple.first |> toString)
                                              ++ "," ++ (s.begin |> Tuple.second |> toString) 
                                              ++ ")-(" ++ (s.end |> Tuple.first |> toString)
                                              ++ "," ++ (s.end |> Tuple.second |> toString) ++ ")"
                                         )
                        |> Maybe.withDefault ""
    in
        div [ id "modeline"
            , style [ ("background-color","black")
                    , ("color", "white")
                    ]
            ] [ text <| toCursorString model.editor.buffer.cursor
              , text <| toIMEString model.editor.compositionData
              , text <| toSelectionString model.editor.buffer.selection
              ]

controlPane : Model -> Html Msg
controlPane model =
    div [ id "control-pane"
        , style [("background-color", "silver")]
        ] [ button [ onClick MoveBackword ] [text "←"]
           , button [ onClick MovePrevios  ] [text "↑"]
           , button [ onClick MoveNext     ] [text "↓"]
           , button [ onClick MoveForward  ] [text "→"]
           , text "|"
           , button [ onClick Backspace ] [ text "BS" ]
           , button [ onClick Delete ] [ text "DEL" ]
           , text "|"
           , button [ onClick Copy ]   [ text "Copy" ]
           , button [ onClick Cut ]   [ text "Cut" ]
           , button [ onClick Pasete ] [ text "Pasete" ]
           , text "|"
           , button [ onClick Undo ]   [ text "Undo" ]
           , text "|"
           , input [ type_ "file", id "file_open" ][]
           ]

debugPane : Model -> Html Msg
debugPane model =
    div [ id "debug-pane"
        , class "hbox"
        , style [ ("display", "flex")
                , ("flex-direction", "row")
                , ("width" , "100%"), ("height", "100%")
                , ("flex-grow", "3")
                , ("min-height", "3em")
                , ("max-height", "8em")
                ]
        ] [ div [ id "debug-pane-history"
                ,style [ ("min-width", "8em")
                        , ("overflow","scroll")
                        , ("flex-grow", "2")
                        ]
                ]
                ( List.map
                          (\ c ->
                               let
                                   pos2str = \ row col -> "(" ++ (toString row) ++ ", " ++ (toString col) ++")" 
                                   celstyle = style [("text-wrap", "none"), ("white-space","nowrap"), ("color", "gray")]
                               in
                                   case c of
                                       Buffer.Cmd_Insert (row, col) str ->
                                           div [celstyle] [ "Ins" ++ (pos2str row col) ++ "{" ++ str ++ "}" |> text ]
                                       Buffer.Cmd_Backspace (row, col) str ->
                                           div [celstyle] [ "Bs_" ++ (pos2str row col) ++ "{" ++ str ++ "}" |> text ]
                                       Buffer.Cmd_Delete (row, col) str ->
                                           div [celstyle] [ "Del" ++ (pos2str row col) ++ "{" ++ str ++ "}" |> text ]
                          ) model.editor.buffer.history
                    )
          , div [ class "vbox"
                , style [ ("flex-grow", "8")
                        , ("display", "flex"), ("flex-direction", "column")
                        ]
                ] [ div [ id "debug-pane-clipboard"
                        , class "hbox"
                        , style [ ("flex-grow", "2")
                                , ("width", "100%")
                                , ("min-height", "2em")
                                , ("display", "flex"), ("flex-direction", "row")
                                ]
                        ]
                        [ div [ style [ ("background-color","gray"), ("color", "white"), ("width", "10ex")] ] [text "clipboard:"]
                        , div [ style [ ("overflow","auto"), ("width", "100%") ]
                              ] ( List.map
                                      (λ ln-> div [ style [("border-bottom", "1px dotted gray"), ("height", "1em")] ] [ text ln ] )
                                      (String.lines model.editor.copyStore)
                                )
                        ]
                  , div [ id "debug-pane-eventlog"
                        , class "hbox"
                        , style [ ("flex-grow", "8")
                                , ("width", "100%")
                                , ("min-height", "2em")
                                , ("display", "flex"), ("flex-direction", "row")
                                ]
                        ]
                        [ div [ style [ ("background-color","gray"), ("color", "white"), ("width", "10ex")] ]
                              [ div [] [text "eventlog:"]
                              , div
                                    [ onClick (SetEventlogEnable (model.editor.event_log == Nothing))
                                    , style [ ("border", "1px solid white")
                                            , ("opacity", if (model.editor.event_log == Nothing) then "0.5" else "1.0" )
                                            , ("margin", "1ex")
                                            , ("text-align", "center")
                                            ]
                                    ]
                                    [text <| if (model.editor.event_log == Nothing) then "OFF" else "ON"]
                              ]
                        , div [ style [ ("overflow","scroll")
                                      , ("width", "calc( 100% - 2px )")
                                      , ("border", "1px solid black")
                                      , ("flex-grow", "8")
                                      ]
                              ]
                              ( List.map (λ ln -> span [ style [("margin-right","0.2em")]] [text ln]) (Maybe.withDefault [] model.editor.event_log) )
                        ]
                  ]
          ]


