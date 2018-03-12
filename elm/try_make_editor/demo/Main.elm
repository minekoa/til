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
    = RawInput String
    | MoveForward
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

init : (Model, Cmd Msg)
init =
    ( Model (Editor.init "editor-sample1" "") ""
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RawInput txt ->
            ( { model
                  | editor = Editor.init model.editor.id txt
              }
            , Cmd.none)
        MoveForward ->
            ( { model
                  | editor = Editor.moveForward model.editor
              }
            , Cmd.none)
        MoveBackword ->
            ( { model
                  | editor = Editor.moveBackward model.editor
              }
            , Cmd.none)
        MovePrevios ->
            ( { model
                  | editor = Editor.movePrevios model.editor
              }
            , Cmd.none)
        MoveNext ->
            ( { model
                  | editor = Editor.moveNext model.editor
              }
            , Cmd.none)

        Backspace ->
            ( { model
                  | editor = Editor.backspace model.editor (Buffer.nowCursorPos model.editor.buffer)
              }
            , Cmd.none)

        Delete ->
            ( { model
                  | editor =  case model.editor.buffer.selection of
                                  Nothing -> Editor.delete model.editor (Buffer.nowCursorPos model.editor.buffer)
                                  Just s  -> Editor.deleteRange model.editor s
              }
            , Cmd.none)

        Copy ->
            ( { model
                  | editor = model.editor.buffer.selection
                                 |> Maybe.andThen (\sel -> Just <| Editor.copy model.editor sel)
                                 |> Maybe.withDefault model.editor
              }
            , Cmd.none)

        Cut ->
            ( { model
                  | editor = model.editor.buffer.selection
                                 |> Maybe.andThen (\sel -> Just <| Editor.cut model.editor sel)
                                 |> Maybe.withDefault model.editor
              }
            , Cmd.none)

        Pasete ->
            ( { model
                  | editor = Editor.paste model.editor (Buffer.nowCursorPos model.editor.buffer) model.editor.copyStore
              }
            , Cmd.none)
        Undo ->
            ( { model
                  | editor = Editor.undo model.editor
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

debugPane : Model -> Html msg
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
                        ] [ div [ style [ ("background-color","gray"), ("color", "white")] ] [text "clipboard:"]
                          , div [ style [ ("overflow","auto"), ("width", "100%") ]
                                ] ( List.map
                                          (λ ln-> div [ style [("border-bottom", "1px dotted gray"), ("height", "1em")] ] [ text ln ] )
                                          (String.lines model.editor.copyStore)
                                  )
                          ]
                  , div [ id "debug-pane-eventlog"
                        , style [ ("overflow","scroll")
                                , ("width", "calc( 100% - 2px )")
                                , ("border", "1px solid black")
                                , ("flex-grow", "8")
                                ]
                        ] ( List.map (λ ln -> span [ style [("margin-right","0.2em")]] [text ln]) model.editor.event_log )
                  ]
          ]


