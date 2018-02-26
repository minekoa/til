module Editor.Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


{-| This module is simple texteditor.

-}

type alias Cursor =
    { row : Int
    , column : Int
    }


type alias Model =
    { cursor : Cursor
    , contents : List String
    , input_buffer : String
    , enableComposer : Bool
    , compositionData : Maybe String --IMEで返還中の未確定文字
    , history : Maybe String
    , event_memo : List String -- for debug
    , focus : Bool
    }

init : String -> Model
init text =
    Model (Cursor 0 0)           -- cursor
          (String.lines text)    -- contents
          ""                     -- input_buffer
          False Nothing          -- COMPOSER STATE
          Nothing                -- history
          []                     -- event_memo
          False                  -- focus


line : Int -> List String -> Maybe String
line n lines =
    if n < 0
    then Nothing
    else List.head (List.drop n lines)

initCursor : List String -> Cursor
initCursor contents =             
    let
        n = List.length contents
    in
        Cursor (if n < 0 then 0 else n) 0

------------------------------------------------------------
-- update
------------------------------------------------------------

type Msg
    = Input String
    | KeyDown Int
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String
    | FocusIn Bool
    | FocusOut Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input s ->
            case model.enableComposer of
                True ->
                    ( { model
                          | input_buffer = s
                      }
                    , Cmd.none )
                False ->
                    ( insert model (model.cursor.row, model.cursor.column) (String.right 1 s)
                      |> inputBufferClear
                      |> eventMemorize ("(" ++ (String.right 1 s) ++ ")")
                    , Cmd.none)

        KeyDown code ->
            keyDown code model

        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            compositionEnd data model

        FocusIn _ ->
            ( {model| focus = True}
            , Cmd.none)
        FocusOut _ ->
            ( {model|focus = False}
            , Cmd.none)


keyDown : Int -> Model -> (Model, Cmd Msg)
keyDown code model =
    case code of
        37 -> -- '←'
            ( moveBackward model
              |> eventMemorize "D"
            , Cmd.none )
        38 -> -- '↑'
            ( movePrevios model
              |> eventMemorize "D"
            , Cmd.none)
        39 -> -- '→'
            ( moveForward model
              |> eventMemorize "D"
            , Cmd.none)
        40 -> -- '↓'
            ( moveNext model
              |> eventMemorize "D"
            , Cmd.none)
        8 -> -- bs
            ( backspace model (model.cursor.row, model.cursor.column)
              |> eventMemorize "D"
            , Cmd.none)
        _ ->
            ( eventMemorize "D" model
            , Cmd.none)

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
      |> inputBufferClear
      |> eventMemorize ("Cs{" ++ data ++ "} ")
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
      |> inputBufferClear
      |> eventMemorize ("Cu{" ++ data ++ "} ")
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    ( insert model (model.cursor.row, model.cursor.column) data
      |> composerDisable
      |> inputBufferClear
      |> eventMemorize ("Ce{" ++ data ++ "} ")
    , Cmd.none
    )

------------------------------------------------------------
-- control state update
------------------------------------------------------------

eventMemorize : String -> Model -> Model
eventMemorize s model =
    { model | event_memo = (s :: model.event_memo) }

inputBufferClear : Model -> Model
inputBufferClear model =
    { model | input_buffer = "" }

composerDisable : Model -> Model
composerDisable model =
    { model
        | compositionData = Nothing
        , enableComposer = False
    }

------------------------------------------------------------
-- cursor
------------------------------------------------------------

moveForward : Model -> Model
moveForward model =
    let
        cur = model.cursor
    in
        line cur.row model.contents 
        |> Maybe.andThen
            ( λ ln -> 
                case (cur.column < (maxColumn ln) + 1, cur.row < maxRow model.contents) of
                    (True , _    ) -> Just {cur| column = cur.column + 1}
                    (False, True ) -> Just {cur| column = 0, row = cur.row +1}
                    (False, False) -> Just cur

            )
        |> Maybe.withDefault (initCursor model.contents)
        |> (λ c -> {model | cursor = c})


moveBackward : Model -> Model
moveBackward model =
    let
        cur = model.cursor
        pln = line (cur.row - 1) model.contents |> Maybe.withDefault ""
    in
        line cur.row model.contents 
        |> Maybe.andThen
            ( λ ln -> 
                case (cur.column > 0, cur.row > 0 ) of
                    (True , _    ) -> Just {cur| column = cur.column - 1}
                    (False, True ) -> Just {cur| column = (String.length pln), row = cur.row - 1}
                    (False, False) -> Just cur

            )
        |> Maybe.withDefault (initCursor model.contents)
        |> (λ c -> {model | cursor = c})



movePrevios : Model -> Model
movePrevios model =
    let
        cur = model.cursor
    in
        line (cur.row - 1) model.contents 
        |> Maybe.andThen
            ( λ ln -> 
                case cur.column < (maxColumn ln) + 1 of
                    True  -> Just {cur| row = cur.row - 1}
                    False -> Just {cur| row = cur.row - 1, column = (maxColumn ln) + 1}
            )
        |> Maybe.withDefault cur
        |> (λ c -> {model | cursor = c})



moveNext : Model -> Model
moveNext model =
    let
        cur = model.cursor
    in
        line (cur.row + 1) model.contents 
        |> Maybe.andThen
            ( λ ln -> 
                case cur.column < (maxColumn ln) + 1 of
                    True  -> Just {cur| row = cur.row + 1}
                    False -> Just {cur| row = cur.row + 1, column = (maxColumn ln) + 1}
            )
        |> Maybe.withDefault cur
        |> (λ c -> {model | cursor = c})


-- Tool

maxColumn: String -> Int
maxColumn line =
    (String.length line) - 1

maxRow : List String -> Int
maxRow contents =
    (List.length contents) - 1


------------------------------------------------------------
-- edit
------------------------------------------------------------


type EditCommand
    = Insert (Int, Int) String
    | Delete (Int, Int)
    


insert: Model -> (Int, Int)  -> String -> Model
insert model (row, col) text =
    let
        contents = model.contents
        prows = List.take row contents
        crow  = line row model.contents |> Maybe.withDefault ""
        nrows = List.drop (row + 1) contents

        texts = (String.lines text)
        left  = (String.left col crow)
        right = (String.dropLeft (col) crow)

        car = List.head >> Maybe.withDefault ""
    in
        case List.length texts of
            0 ->
                model
            1 ->
                { model
                    | contents = prows ++ ((left ++ text ++ right) :: nrows)
                    , cursor = Cursor row (col + (String.length text))
                }
            2 ->
                let
                    fst_ln = car texts
                    lst_ln = car <| List.drop 1 texts
                in
                    { model
                        | contents = prows ++ [ left ++ fst_ln, lst_ln ++ right]
                                           ++ nrows
                         , cursor = Cursor (row + 1) (String.length lst_ln)
                    }
            n ->
                let
                    fst_ln = car texts
                    lst_ln = car <| List.drop (n - 1) texts
                in
                    { model
                        | contents = prows ++ [ left ++ fst_ln ] ++ (List.drop 1 (List.take (n - 1) texts)) ++ [lst_ln ++ right]
                                           ++ nrows
                        , cursor = Cursor (row + n - 1) (String.length lst_ln)
                    }


backspace: Model -> (Int, Int) -> Model
backspace model (row, col) =
    case (row, col) of
        (0, 0) ->
            model
        (_, 0) ->
            let
                prows  = List.take (row - 1) model.contents
                crow   = List.drop (row - 1) model.contents |> List.take 2 |> String.concat
                nrows  = List.drop (row + 1) model.contents

                n_col  = List.drop (row - 1) model.contents |> List.head |> Maybe.withDefault "" |> String.length
            in
                { model
                    | contents = prows ++ (crow :: nrows )
                    , cursor = Cursor (row - 1) (n_col)
                }
        (_, n) ->
            let
                prows = List.take row model.contents
                crow  = line row model.contents |> Maybe.withDefault ""
                nrows = List.drop (row + 1) model.contents

                left  = (String.left (col - 1) crow)
                right = (String.dropLeft (col) crow)
            in
                { model
                    | contents = prows ++ ((left ++ right) :: nrows)
                    , cursor = Cursor row  (col - 1)
                }

------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [class "editor"]
        [ presentation model ]

presentation : Model -> Html Msg
presentation model =
    div [ style [ ("display", "flex"), ("flex-direction", "row"), ("flex-wrap", "no-wrap")
                , ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("position", "relative")
                ]
        , onFocusIn FocusIn
        , onFocusOut FocusOut
        ]
        [ lineNumArea model
        , codeArea model
        ]

lineNumArea : Model -> Html Msg
lineNumArea model =
    let
        contents = model.contents
    in
        div [ class "line-num-area"
            , style [ ("text-align", "right")
                    , ("padding-right", "0.2em")]
            ] <|
            List.map
                (λ n -> div [ class "line-num"
                             , style [ ("height", "1em")
                                     , ("text-wrap", "none")]
                             ] [ text (toString n) ])
                (List.range 1 (List.length contents))

codeArea : Model -> Html Msg
codeArea model =
    div [ class "code-area" ]
        [ cursorLayer2 model
        , codeLayer model
        ]

codeLayer: Model  -> Html Msg
codeLayer model = 
    let
        contents = model.contents
        cursor = model.cursor
    in
        div [class "lines"] <|
            List.indexedMap
                (λ n ln ->
                      if n == cursor.row then
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
--                                      , ("display" , "inline-flex")
                                      ]
                              ]
                              [ span [ style [ ("position", "relative")
                                             , ("white-space", "pre")
                                             ]
                                     ]
                                    [ text <| String.left cursor.column ln]
                              , compositionPreview model.compositionData
                              , span [ style [ ("position", "relative")
                                             , ("white-space", "pre")
                                             ]
                                     ]
                                    [ text <| String.dropLeft cursor.column ln]                                  
                              ]
                      else
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
                                      ]
                              ] [text ln] ) contents

cursorLayer2 : Model -> Html Msg
cursorLayer2 model =
    {- 作戦2, [パディング用要素、本物の文字を入れ、hiddenにする][cursol]
       思いついたのでやってみたらうまく行った
       けど、みんなこれをやってないの、なにか落とし穴あるからなのかな？
    -}
    div [ class "cursors"
        , style [("position", "absolute")]
        ]
        [ div [style [ ("position", "relative")
                     , ("display" , "inline-flex")
                     , ("flex-direction", "row")
                     , ("flex-wrap", "nowrap")
                     , ("justify-content", "flex-start")
                     , ("height", "1em")

                     , ("top" , (model.cursor.row |> toString) ++ "em")
                     , ("left", "0")
                     ]
               ]
               [ ruler model
               , div
                     [ style [("position", "relative"), ("display" , "inline-flex")] ]
                     [ textarea [ id "input-control"
                                , onInput Input
                                , onKeyDown KeyDown
                                , onCompositionStart CompositionStart
                                , onCompositionUpdate CompositionUpdate
                                , onCompositionEnd CompositionEnd
                                , value model.input_buffer
                                , style [ ("width", "1px"), ("border", "none"), ("padding", "none"), ("margin","none"), ("outline", "none")
                                        , ("overflow", "hidden"), ("opacity", "0")
                                        , ("resize", "none")
                                        , ("position", "absolute") -- textarea のサイズは（入力を取れる状態を維持したままでは）0にできないので、カーソル位置がずれぬよう、浮かせてあげる
                                        ]
                                , spellcheck False
                                , wrap "off"
                                ]
                           []
                     , span [ style [("visibility", "hidden") ]] [compositionPreview model.compositionData]
                     , cursorView model
                     ]
               ]
        ]


ruler : Model -> Html msg
ruler model = 
    let
        cur = model.cursor
        contents = model.contents
    in
    span [ id "ruler"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ line cur.row contents |> Maybe.withDefault "" |> String.left cur.column |> text ]

compositionPreview : Maybe String -> Html msg
compositionPreview compositionData =
    case compositionData of
        Just s ->
            span [ class "composition_data"
                 , style [ ("color", "blue")
                         , ("text-decoration", "underline")
                         ]
                 ] [ text s ]
        Nothing ->
            text ""

cursorView : Model -> Html msg
cursorView model =
    span [style [ ("background-color", if model.focus then "blue" else "gray" )
                , ("opacity", "0.5")
                , ("height", "1em")
                , ("width", "3px")
                ]
         ]
    []
    

------------------------------------------------------------
-- html events (extra)
------------------------------------------------------------
                
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (Json.map tagger keyCode)

onKeyUp: (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)

onCompositionStart: (String -> msg) -> Attribute msg
onCompositionStart tagger =
    on "compositionstart" (Json.map tagger (Json.field "data" Json.string))

onCompositionEnd: (String -> msg) -> Attribute msg
onCompositionEnd tagger =
    on "compositionend" (Json.map tagger (Json.field "data" Json.string))

onCompositionUpdate: (String -> msg) -> Attribute msg
onCompositionUpdate tagger =
    on "compositionupdate" (Json.map tagger (Json.field "data" Json.string))

onFocusIn : (Bool -> msg) -> Attribute msg
onFocusIn tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusin" (Json.map tagger (Json.field "bubbles" Json.bool))

onFocusOut : (Bool -> msg) -> Attribute msg
onFocusOut tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusout" (Json.map tagger (Json.field "bubbles" Json.bool))



