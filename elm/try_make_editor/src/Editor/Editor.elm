module Editor.Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Time exposing (Time, second)


import Native.Mice

{-| This module is simple texteditor.

-}

type alias Model =
    { id : String -- frame id

    -- buffer
    , cursor : Cursor
    , contents : List String
    , history : List EditCommand

    -- frame
    , input_buffer : String
    , enableComposer : Bool
    , compositionData : Maybe String --IMEで返還中の未確定文字
    , focus : Bool
    , blink : BlinkState

    -- for debug
    , event_memo : List String
    }

init : String -> String -> Model
init id text =
    Model id                     -- id

          (Cursor 0 0)           -- cursor
          (String.lines text)    -- contents
          []                     -- history

          ""                     -- input_buffer
          False Nothing          -- COMPOSER STATE
          False                  -- focus
          BlinkBlocked           -- blink

          []                     -- event_memo


-- frame > cursor blink

type BlinkState
    = Blink Bool
    | BlinkBlocked

blinkTransition : BlinkState -> BlinkState
blinkTransition blnk =
    case blnk of
        BlinkBlocked -> Blink True
        Blink True   -> Blink False
        Blink False  -> Blink True

blinkBlock : Model -> Model
blinkBlock model =
    {model | blink = BlinkBlocked}


-- buffer > cursor

type alias Cursor =
    { row : Int
    , column : Int
    }

initCursor : List String -> Cursor
initCursor contents =             
    let
        n = List.length contents
    in
        Cursor (if n < 0 then 0 else n) 0


-- buffer > contents

line : Int -> List String -> Maybe String
line n lines =
    if n < 0
    then Nothing
    else List.head (List.drop n lines)

maxColumn: String -> Int
maxColumn line =
    (String.length line) - 1

maxRow : List String -> Int
maxRow contents =
    (List.length contents) - 1

-- buffer > history (for undo/redo)

type EditCommand
    = Cmd_Insert (Int, Int) String
    | Cmd_Backspace (Int, Int) String -- str は undo用
    | Cmd_Delete (Int, Int) String    -- str は undo 用
--    | Cmd_Undo EditCommand

appendHistory: EditCommand -> Model -> Model
appendHistory cmd model =
    case (cmd, List.head model.history) of
        ( (Cmd_Insert (r, c) s), Just (Cmd_Insert (or, oc) os) ) ->
            if (r == or) && (c == oc + (String.length os))
            then { model | history = (Cmd_Insert (or, oc) (os ++ s)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        ( (Cmd_Backspace (r, c) s), Just (Cmd_Backspace (or, oc) os) ) ->
            if (r == or) && (c == oc - (String.length os))
            then { model | history = (Cmd_Backspace (or, oc) (s ++ os)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        ( (Cmd_Delete (r, c) s), Just (Cmd_Delete (or, oc) os) ) ->
            if (r == or) && (c == oc)
            then { model | history = (Cmd_Delete (or, oc) (os ++ s)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        (_ , _) ->
            { model | history = cmd :: model.history }



------------------------------------------------------------
-- update
------------------------------------------------------------

type Msg
    = Input String
    | KeyDown KeyboardEvent
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String
    | FocusIn Bool
    | FocusOut Bool
    | SetFocus
    | Tick Time

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
        SetFocus ->
            ( {model | focus = doFocus (model.id ++ "-input")}
            , Cmd.none )

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )



keyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
keyDown e model =
    case e.keyCode of
        37 -> -- '←'
            ( moveBackward model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none )
        38 -> -- '↑'
            ( movePrevios model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        39 -> -- '→'
            ( moveForward model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        40 -> -- '↓'
            ( moveNext model
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        8 -> -- bs
            ( backspace model (model.cursor.row, model.cursor.column)
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        46 -> -- del
            ( delete model (model.cursor.row, model.cursor.column)
              |> eventMemorize ("D:" ++ keyboarEvent_toString e)
            , Cmd.none)
        90 -> -- 'Z'
            -- note: ためしに Ctrl-Z で undo を実装したが、
            --       textareaの持つ undo機能が発動し、
            --       （空文字の）input がされてしまう不具合を発見.
            --       結果、まだ「使えない」実装に。
            --       どう対処したものか...
            if (e.ctrlKey && not e.altKey  && not e.metaKey && not e.shiftKey)
            then
                ( undo model
                  |> eventMemorize ("D:*" ++ keyboarEvent_toString e)
                , Cmd.none)
            else
                ( model
                  |> eventMemorize ("D:"++ keyboarEvent_toString e)
                , Cmd.none)
        _ ->
            ( model
              |> eventMemorize ("D:"++ keyboarEvent_toString e)
            , Cmd.none)

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
      |> inputBufferClear
      |> blinkBlock
      |> eventMemorize ("Cs{" ++ data ++ "} ")
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
      |> inputBufferClear
      |> blinkBlock
      |> eventMemorize ("Cu{" ++ data ++ "} ")
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    ( insert model (model.cursor.row, model.cursor.column) data
      |> composerDisable
      |> inputBufferClear
      |> blinkBlock
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
        |> blinkBlock


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
        |> blinkBlock



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
        |> blinkBlock



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
        |> blinkBlock



------------------------------------------------------------
-- edit
------------------------------------------------------------

insert: Model -> (Int, Int)  -> String -> Model
insert model (row, col) text =
    insert_proc model (row, col) text
    |> appendHistory (Cmd_Insert (row, col) text)
    |> blinkBlock

insert_proc: Model -> (Int, Int)  -> String -> Model
insert_proc model (row, col) text =
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
    let
        (m, deleted) = backspace_proc model (row, col)
    in
        case deleted of
            Nothing ->
                m
                |> blinkBlock
            Just s ->
                m
                |> appendHistory (Cmd_Backspace (row, col) s)
                |> blinkBlock



backspace_proc: Model -> (Int, Int) -> (Model, Maybe String)
backspace_proc model (row, col) =
    case (row, col) of
        (0, 0) ->
            (model, Nothing)

        (_, 0) ->
            let
                prows  = List.take (row - 1) model.contents
                crow   = List.drop (row - 1) model.contents |> List.take 2 |> String.concat
                nrows  = List.drop (row + 1) model.contents

                n_col  = List.drop (row - 1) model.contents |> List.head |> Maybe.withDefault "" |> String.length
            in
                ( { model
                      | contents = prows ++ (crow :: nrows )
                      , cursor = Cursor (row - 1) (n_col)
                  }
                , Just "\n")

        (_, n) ->
            let
                prows = List.take row model.contents
                crow  = line row model.contents |> Maybe.withDefault ""
                nrows = List.drop (row + 1) model.contents

                left  = (String.left (col - 1) crow)
                right = (String.dropLeft (col) crow)
            in
                ( { model
                      | contents = prows ++ ((left ++ right) :: nrows)
                      , cursor = Cursor row  (col - 1)
                  }
                , Just (crow |> String.dropLeft (col - 1) |> String.left 1) )


delete: Model -> (Int, Int) -> Model
delete model (row, col) =
    let
        (m, deleted) = delete_proc model (row, col)
    in
        case deleted of
            Nothing ->
                m
                |> blinkBlock
            Just s ->
                m
                |> appendHistory (Cmd_Delete (row, col) s)
                |> blinkBlock



delete_proc: Model -> (Int, Int) -> (Model, Maybe String)
delete_proc model (row, col) =
    let
        ln      = line row model.contents |> Maybe.withDefault ""
        max_row = maxRow model.contents
        max_col = maxColumn ln
    in
        case (row == max_row, col > max_col) of
             (True, True)  ->
                 (model , Nothing)

             (_   , False) ->
                 let
                     prows  = List.take row model.contents
                     nrows  = List.drop (row + 1) model.contents

                     current = (String.left (col) ln) ++ (String.dropLeft (col + 1) ln)
                 in
                     ( { model
                           | contents = prows ++ (current :: nrows)
                       }
                     , Just (ln |> String.dropLeft col |> String.left 1) )

             (_   , True) ->
                 let
                     prows  = List.take row model.contents
                     nxt    = line (row + 1) model.contents |> Maybe.withDefault ""
                     nrows  = List.drop (row + 2) model.contents

                     current = ln ++ nxt
                 in
                     ( { model
                           | contents = prows ++ (current :: nrows)
                       }
                     , Just "\n" )


undo : Model -> Model
undo model =
    -- todo: ちゃんと実装する。
    --       現存の編集イベントを組み合わせて強引に実現している。汚い。
    ( case List.head model.history of
          Nothing -> model
          Just cmd ->
              ( case cmd of
                    Cmd_Insert (row, col) str    ->
                        let
                            delete_n = (\ c m ->
                                            if c <= 0 then m
                                            else backspace_proc m (m.cursor.row, m.cursor.column)
                                                 |> Tuple.first
                                                 |> delete_n (c - 1)
                                       )
                            ls      = String.lines str
                            r_delta = (List.length ls) - 1
                            c_delta = ls
                                      |> List.reverse 
                                      |> List.head
                                      |> Maybe.withDefault ""
                                      |> String.length
                                      |> (+) -1
                        in
                            delete_n (String.length str)
                                { model
                                    | cursor = (Cursor (row + r_delta) (col + c_delta + 1))
                                }
                             |> eventMemorize ("<undo(ins:" ++ str ++ ")>")

                    Cmd_Backspace (row, col) str ->
                        let
                            ls      = String.lines str
                            r_delta = (List.length ls) - 1
                            c_delta = ls
                                      |> List.reverse 
                                      |> List.head
                                      |> Maybe.withDefault ""
                                      |> String.length
                                      |> (+) -1
                        in
                            insert_proc model (row - r_delta, col - c_delta - 1) str
                            |> eventMemorize "<undo(bs)>"

                    Cmd_Delete (row, col) str    ->
                        insert_proc model (row, col) str
                        |> eventMemorize "<undo(del)>"

              )
              |> (\ m -> {m | history = List.drop 1 m.history })
    )
    |> blinkBlock

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
        , onClick SetFocus
--        , contenteditable True
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
                     [ textarea [ id <| model.id ++ "-input"
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
    let
        blink_off = (\blnk -> case blnk of
                                Blink b      -> b
                                BlinkBlocked -> True
                    ) >> not
    in
    span [style [ ("background-color", if model.focus then "blue" else "gray" )
                , ("opacity", if model.focus && (blink_off model.blink) then "0.0" else "0.5")
                , ("height", "1em")
                , ("width", "3px")
                ]
         ]
    []


------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch [ Time.every (0.5 * second) Tick ]


------------------------------------------------------------
-- html events (extra)
------------------------------------------------------------

-- Keyboard Event

type alias KeyboardEvent = 
    { altKey : Bool
    , ctrlKey : Bool
    , keyCode : Int
    , metaKey : Bool
    , repeat : Bool
    , shiftKey : Bool
    }

keyboarEvent_toString : KeyboardEvent -> String
keyboarEvent_toString e =
    String.concat
        [ if e.ctrlKey then "C-" else ""
        , if e.altKey then "A-" else ""
        , if e.metaKey then "M-" else ""
        , if e.shiftKey then "S-"else ""
        , toString e.keyCode
        ]

decodeKeyboardEvent : Json.Decoder KeyboardEvent
decodeKeyboardEvent =
    Json.map6 KeyboardEvent
        (Json.field "altKey" Json.bool)
        (Json.field "ctrlKey" Json.bool)
        (Json.field "keyCode" Json.int)
        (Json.field "metaKey" Json.bool)
        (Json.field "repeat" Json.bool)
        (Json.field "shiftKey" Json.bool)    

                
onKeyDown : (KeyboardEvent -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger decodeKeyboardEvent)

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (Json.map tagger keyCode)

onKeyUp: (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


-- Composition Event (IME)

onCompositionStart: (String -> msg) -> Attribute msg
onCompositionStart tagger =
    on "compositionstart" (Json.map tagger (Json.field "data" Json.string))

onCompositionEnd: (String -> msg) -> Attribute msg
onCompositionEnd tagger =
    on "compositionend" (Json.map tagger (Json.field "data" Json.string))

onCompositionUpdate: (String -> msg) -> Attribute msg
onCompositionUpdate tagger =
    on "compositionupdate" (Json.map tagger (Json.field "data" Json.string))

-- Focus Event

onFocusIn : (Bool -> msg) -> Attribute msg
onFocusIn tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusin" (Json.map tagger (Json.field "bubbles" Json.bool))

onFocusOut : (Bool -> msg) -> Attribute msg
onFocusOut tagger =
    -- ほしいプロパティはないのでとりあえずダミーで bubbles を
    on "focusout" (Json.map tagger (Json.field "bubbles" Json.bool))


------------------------------------------------------------
-- Native (Mice)
------------------------------------------------------------

doFocus : String -> Bool
doFocus id = Native.Mice.doFocus id

