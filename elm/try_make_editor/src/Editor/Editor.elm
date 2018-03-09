module Editor.Editor exposing ( Model
                              , init
                              , update
                              , Msg(..)
                              , moveForward
                              , moveBackward
                              , movePrevios
                              , moveNext
                              , selectForward
                              , selectBackward
                              , selectPrevios
                              , selectNext
                              , insert
                              , backspace
                              , delete
                              , deleteRange
                              , undo
                              , copy
                              , cut
                              , paste
                              , subscriptions
                              , view
                              )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Mouse
import Time exposing (Time, second)
import Task exposing (Task)

import Editor.Buffer as Buffer
import Native.Mice

{-| This module is simple texteditor.

-}


type alias Model =
    { id : String -- frame id

    , buffer : Buffer.Model

    -- ?
    , copyStore : String

    -- frame
    , input_buffer : String
    , enableComposer : Bool
    , compositionData : Maybe String --IMEで返還中の未確定文字
    , focus : Bool
    , blink : BlinkState

    -- for debug
    , event_log : List String
--    , xy : Mouse.Position

    -- work
    , isPreventedDefaultKeyShortcut : Bool
    , copyReq : Maybe String
    , vScrollReq : Maybe Int
    , hScrollReq : Maybe Int
    }

init : String -> String -> Model
init id text =
    Model id                     -- id
          (Buffer.init text)
          ""                     -- copyStore
          ""                     -- input_buffer
          False Nothing          -- COMPOSER STATE
          False                  -- focus
          BlinkBlocked           -- blink

          []                     -- event_log
--          (Mouse.Position 0 0)
          False                  --preventedKeyShortcut
          Nothing                --copyReq
          Nothing                --scrollReq (V)
          Nothing                --scrollReq (H)

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


requestCopyToClipboard : String -> Model -> Model
requestCopyToClipboard s model =
    {model | copyReq = Just s}


------------------------------------------------------------
-- update
------------------------------------------------------------

type Msg
    = IgnoreResult
    | PreventDefaultKeyShortcut Bool
    | Copied Bool
    | Pasted String
    | ScrolledV Bool
    | ScrolledH Bool
    | Input String
    | KeyDown KeyboardEvent
    | CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String
    | FocusIn Bool
    | FocusOut Bool
    | SetFocus
    | DragStart Int Mouse.Position
    | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- Task Result (Native)

        IgnoreResult ->
            (model, Cmd.none)

        PreventDefaultKeyShortcut b ->
            ( {model | isPreventedDefaultKeyShortcut = b}, Cmd.none)

        Copied _ ->
            ( {model | copyReq = Nothing}
            , Task.perform (\_ -> IgnoreResult) (doFocus <| model.id ++ "-input"))

        Pasted s ->
            ( paste model (Buffer.nowCursorPos model.buffer) s
              |> eventLog "pasete" s
            , Cmd.none
            )

        ScrolledV _ ->
            ( {model | vScrollReq = Nothing }
            , Cmd.none )

        ScrolledH _ ->
            ( {model | hScrollReq = Nothing }
            , Cmd.none )


        -- View Operation Event

        Input s ->
            let
                (m, c) = input s model
            in
                ( m
                , Cmd.batch [ c
                            , case m.vScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledV (setScrollTop (model.id ++ "-editor-frame") n )
                            , case m.hScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledH (setScrollLeft (model.id ++ "-editor-frame") n )
                            ]
                )

        KeyDown code ->
            let 
                (m, c) = keyDown code model
            in
                ( m
                , Cmd.batch [ c
                            , case m.copyReq of
                                  Nothing -> Cmd.none
                                  Just s  -> Task.perform Copied (copyToClipboard (model.id ++ "-clipboard-copy") s)
                            , case m.vScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledV (setScrollTop (model.id ++ "-editor-frame") n )
                            , case m.hScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledH (setScrollLeft (model.id ++ "-editor-frame") n )
                            ]
                )
 

        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            let
                (m, c) =compositionEnd data model
            in
                ( m
                , Cmd.batch [ c
                            , case m.vScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledV (setScrollTop (model.id ++ "-editor-frame") n)
                            , case m.hScrollReq of
                                  Nothing -> Cmd.none
                                  Just n  -> Task.perform ScrolledH (setScrollLeft (model.id ++ "-editor-frame") n )
                            ]
                )

        FocusIn _ ->
            ( {model
                  | focus = True
                  , isPreventedDefaultKeyShortcut = True
              }

            , if model.isPreventedDefaultKeyShortcut then
                  Cmd.none
              else
                  Task.perform PreventDefaultKeyShortcut (elaborateInputAreaEventHandlers (model.id ++ "-input") (model.id ++ "-clipboard-paste"))
            )

        FocusOut _ ->
            ( {model|focus = False}
            , Cmd.none)

        SetFocus ->
            ( model
                |> eventLog "setfocus" ""
            , Task.perform (\_ -> IgnoreResult) (doFocus <| model.id ++ "-input"))

        DragStart row xy ->
            let
                calc_w  = calcTextWidth (model.id ++ "-ruler")
                calc_col = (\ ln c x ->
                              if (calc_w (String.left c ln)) > x || String.length ln < c  then c - 1
                              else calc_col ln (c + 1)  x)

                ln = Buffer.line row model.buffer.contents |> Maybe.withDefault ""
                rect = getBoundingClientRect (model.id ++ "-codeLayer")

                col = (calc_col ln 0 (xy.x - rect.left))

                b1 = model.buffer
                b2 = { b1 | cursor = Buffer.Cursor row col }


            in
--                ( { model | buffer = b2, xy = xy}
                ( { model | buffer = b2 }
                  |> eventLog "dragstart" ("pos=" ++ (toString xy.x) ++ "," ++ (toString xy.y)
                                               ++ "; offsetx=" ++ (toString (xy.x - rect.left))
                                               ++ "; row=" ++ (toString row)
                                               ++ "; calced_col=" ++ (toString col)
                                          )
                  |> blinkBlock
                , Cmd.none )

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )



input: String -> Model -> (Model, Cmd Msg)
input s model =
    case model.enableComposer of
        True ->
            ( { model
                  | input_buffer = s
              }
            , Cmd.none )
        False ->
            ( insert model (Buffer.nowCursorPos model.buffer) (String.right 1 s)
                |> inputBufferClear
                |> eventLog "input" (String.right 1 s)
            , Cmd.none)


keymapper : (Bool, Bool, Bool, Int) -> (Model -> Model)
keymapper (ctrl, alt, shift, keycode) =
    let
        keymap = [ {ctrl=False, alt=False, shift=False, code= 37, f=moveBackward } -- '←'
                 , {ctrl=False, alt=False, shift=False, code= 38, f=movePrevios }  -- '↑'
                 , {ctrl=False, alt=False, shift=False, code= 39, f=moveForward }  -- '→'
                 , {ctrl=False, alt=False, shift=False, code= 40, f=moveNext }     -- '↓'

                 , {ctrl=False, alt=False, shift=True , code= 37, f=selectBackward } -- 'S-←'
                 , {ctrl=False, alt=False, shift=True , code= 38, f=selectPrevios }  -- 'S-↑'
                 , {ctrl=False, alt=False, shift=True , code= 39, f=selectForward }  -- 'S-→'
                 , {ctrl=False, alt=False, shift=True , code= 40, f=selectNext }     -- 'S-↓' 

                 , {ctrl=False, alt=False, shift=False, code=  8, f=(\m -> backspace m (Buffer.nowCursorPos m.buffer)) } -- BS
                 , {ctrl=False, alt=False, shift=False, code= 46, f=(\m -> case m.buffer.selection of
                                                                               Nothing -> delete m (Buffer.nowCursorPos m.buffer)
                                                                               Just s  -> deleteRange m s 
                                                                    )  }   -- DEL

                 , {ctrl=True , alt=False, shift=False, code= 67, f=(\m -> m.buffer.selection |> Maybe.andThen (\sel -> Just <| copy m sel) |> Maybe.withDefault m) } -- 'C-c'
                 , {ctrl=True , alt=False, shift=False, code= 88, f=(\m -> m.buffer.selection |> Maybe.andThen (\sel -> Just <| cut m sel) |> Maybe.withDefault m) } -- 'C-x'
                 -- C-v は、クリップボードと連携したいので、ブラウザのpasteイベントを発火させる、ので、ここでは何もしない

                 , {ctrl=True , alt=False, shift=False, code= 90, f= undo }

                 -- emacs like binds
                 , {ctrl=True , alt=False, shift=False, code= 70, f=moveForward } --  'C-f'
                 , {ctrl=True , alt=False, shift=False, code= 66, f=moveBackward }--  'C-b'
                 , {ctrl=True , alt=False, shift=False, code= 78, f=moveNext }    --  'C-n'
                 , {ctrl=True , alt=False, shift=False, code= 80, f=movePrevios } --  'C-p'
                 , {ctrl=True , alt=False, shift=False, code= 72, f=(\m -> backspace m (Buffer.nowCursorPos m.buffer)) }  -- 'C-h'
                 , {ctrl=True , alt=False, shift=False, code= 68, f=(\m -> case m.buffer.selection of
                                                                               Nothing -> delete m (Buffer.nowCursorPos m.buffer)
                                                                               Just s  -> deleteRange m s
                                                                    )  }    -- 'C-d'
                 , {ctrl=True , alt=False, shift=False, code= 77, f=(\m -> insert m (Buffer.nowCursorPos m.buffer) "\n") }-- 'C-m'
                 , {ctrl=True , alt=False, shift=False, code= 89, f=(\m -> paste m (Buffer.nowCursorPos m.buffer) m.copyStore)} -- 'C-y'
                 ]

        search = (\ (ctrl, alt, shift, keycode) l ->
                      case l of
                          [] ->
                              Nothing
                          x :: xs ->
                              if (keycode == x.code)
                                  && (ctrl == x.ctrl) && (alt == x.alt) && (shift == x.shift)
                              then Just x.f
                              else search (ctrl, alt, shift, keycode) xs
                 )
    in
        -- note: 4つ以上の組でのパターンマッチを記述するとビルド時間が非常に長くなる問題があるので、(elm 0.18)
        --       パターンマッチでは書かない

        -- note: Dict にすべきか悩んだが、
        --       モディファイヤ * を導入するかも、なのと
        --       複数のkeymap を合成して早いものがちマッチとかやるかも、なのと
        --       nストロークキーマッチをやるかも、なので
        --       線形サーチで書いている
        search (ctrl, alt, shift, keycode) keymap
            |> Maybe.withDefault (\m -> m)
                                  

keyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
keyDown e model =
    ( keymapper (e.ctrlKey, e.altKey, e.shiftKey, e.keyCode) model
      |> eventLog "keydown" (keyboarEvent_toString e)
    , Cmd.none )

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    ( { model
          | compositionData = Just data
          , enableComposer = True
      }
      |> inputBufferClear -- 直前にenterでない確定をした場合向け
      |> blinkBlock
      |> eventLog "compositoinstart" data
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionData = Just data }
      |> blinkBlock
      |> eventLog "compositionupdate" data
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    ( insert model (Buffer.nowCursorPos model.buffer) data
      |> composerDisable
      |> inputBufferClear
      |> blinkBlock
      |> eventLog "compositionend" data
    , Cmd.none
    )

------------------------------------------------------------
-- control state update
------------------------------------------------------------

eventLog : String -> String -> Model -> Model
eventLog ev data model =
    let
        s = "(" ++ ev ++ ":" ++ data ++ ") "
    in
        { model | event_log = s :: model.event_log }

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
    { model | buffer = Buffer.moveForward model.buffer }
        |> selectionClear
        |> blinkBlock
        |> requestScroll

moveBackward : Model -> Model
moveBackward model =
    { model | buffer = Buffer.moveBackward model.buffer }
        |> selectionClear
        |> blinkBlock
        |> requestScroll

movePrevios : Model -> Model
movePrevios model =
    { model | buffer = Buffer.movePrevios model.buffer }
        |> selectionClear
        |> blinkBlock
        |> requestScroll

moveNext : Model -> Model
moveNext model =
    { model | buffer = Buffer.moveNext model.buffer }
        |> selectionClear
        |> blinkBlock
        |> requestScroll

------------------------------------------------------------
-- scroll
------------------------------------------------------------

requestScroll : Model -> Model
requestScroll = requestVScroll >> requestHScroll

requestVScroll : Model -> Model
requestVScroll model =
    let
        frameRect  = getBoundingClientRect <| model.id ++ "-editor-frame"
        cursorRect = getBoundingClientRect <| model.id ++ "-cursor"
        scrtop = getScrollTop (model.id ++ "-editor-frame")

        margin = cursorRect.height * 3
    in
        if  cursorRect.top - margin < frameRect.top then
            { model | vScrollReq = Just ( scrtop + (cursorRect.top - frameRect.top ) - margin) }
        else
            if  cursorRect.bottom + margin > frameRect.bottom then
                { model | vScrollReq = Just ( scrtop + (cursorRect.bottom - frameRect.bottom ) + margin) }
            else 
                { model | vScrollReq = Nothing}

requestHScroll : Model -> Model
requestHScroll model =
    let
        frameRect  = getBoundingClientRect <| model.id ++ "-editor-frame"
        cursorRect = getBoundingClientRect <| model.id ++ "-cursor"
        scrleft    = getScrollLeft (model.id ++ "-editor-frame")

        margin = cursorRect.height * 3
    in
        if  cursorRect.left - margin < frameRect.left then
            { model | hScrollReq = Just ( scrleft + (cursorRect.left - frameRect.left ) - margin) }
        else
            if  cursorRect.right + margin > frameRect.right then
                { model | hScrollReq = Just ( scrleft + (cursorRect.right - frameRect.right ) + margin) }
            else 
                { model | hScrollReq = Nothing}



printVScrollInfo : Model -> String
printVScrollInfo model = 
    let
        getOffsetTop = \id -> getBoundingClientRect id |> .top

        frameOffset  = getOffsetTop <| model.id ++ "-editor-frame"
        sceneOffset  = getOffsetTop <| model.id ++ "-editor-scene"
        cursorOffset = getOffsetTop <| model.id ++ "-cursor"
        scrollTop    = getScrollTop <| model.id ++ "-editor-frame"
    in
        "offset<" ++ (toString (cursorOffset - frameOffset))
            ++ ">(cur:" ++ (toString cursorOffset)
            ++ ", sce:" ++ (toString sceneOffset)
            ++ ", frm:" ++ (toString frameOffset) ++") "
            ++ "scroll<" ++ (toString scrollTop) ++ "> "


------------------------------------------------------------
-- selection
------------------------------------------------------------

selectionClear : Model -> Model
selectionClear model =
    { model | buffer = Buffer.selectionClear model.buffer }

selectBackward: Model -> Model
selectBackward model =
    model
        |> (\ m -> { m | buffer = Buffer.selectBackward m.buffer })
        |> blinkBlock
        |> requestScroll

selectForward: Model -> Model
selectForward model =
    model
        |> (\m -> { m | buffer = Buffer.selectForward m.buffer })
        |> blinkBlock
        |> requestScroll

selectPrevios: Model -> Model
selectPrevios model =
    model
        |> (\m -> { m | buffer = Buffer.selectPrevios m.buffer })
        |> blinkBlock
        |> requestScroll

selectNext: Model -> Model
selectNext model =
    model
        |> (\m -> { m | buffer = Buffer.selectNext m.buffer })
        |> blinkBlock
        |> requestScroll


------------------------------------------------------------
-- edit
------------------------------------------------------------

insert: Model -> (Int, Int)  -> String -> Model
insert model (row, col) text =
    { model | buffer = Buffer.insert (row, col) text model.buffer }
        |> selectionClear
        |> blinkBlock


backspace: Model -> (Int, Int) -> Model
backspace model (row, col) =
    { model | buffer = Buffer.backspace (row, col) model.buffer}
        |> selectionClear
        |> blinkBlock

delete: Model -> (Int, Int) -> Model
delete model (row, col) =
    { model | buffer = Buffer.delete (row, col) model.buffer}
        |> selectionClear
        |> blinkBlock

deleteRange: Model -> Buffer.Range -> Model
deleteRange model selection =
    { model | buffer = Buffer.deleteRange selection model.buffer}
        |> selectionClear
        |> blinkBlock

undo : Model -> Model
undo model =
    { model | buffer = Buffer.undo model.buffer }
        |> selectionClear
        |> blinkBlock

copy : Model -> Buffer.Range -> Model
copy model selection =
    let
        s = Buffer.readRange selection model.buffer
    in
        { model | copyStore = s }
            |> requestCopyToClipboard s
            |> selectionClear
            |> blinkBlock

cut : Model -> Buffer.Range -> Model
cut model selection =
    let
        s = Buffer.readRange selection model.buffer
    in
        { model | copyStore = s
                , buffer = Buffer.deleteRange selection model.buffer
        }
        |> requestCopyToClipboard s
        |> selectionClear
        |> blinkBlock

paste : Model -> (Int, Int) -> String -> Model
paste model (row, col) text =
    { model | buffer = Buffer.insert (row, col) text model.buffer 
            , copyStore = text  -- clipboard経由のペーストもあるので、copyStoreを更新しておく
    }
    |> selectionClear
    |> blinkBlock


------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ id (model.id ++ "-editor-frame")
        , style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("overflow","auto")
                , ( "position", "relative")
                ]
        ]
        [ div [ id (model.id ++ "-editor-scene")
              , class "editor-scene"
              , style [ ( "position", "relative") ]
              ]
              [ presentation model 
              , controller model
              ]
        ]

controller : Model -> Html Msg
controller model =
    div [ style [ ("width", "1px"), ("height", "1px")
                , ("overflow", "hidden")
                , ("position", "absolute")
                ]
        ]
        [ textarea [ id <| model.id ++ "-clipboard-copy"
                   , tabindex -1
                   ] []
        , textarea [ id <| model.id ++ "-clipboard-paste"
                   , onInput Pasted
                   , tabindex -1
                   ] []
        ]


presentation : Model -> Html Msg
presentation model =
    div [ style [ ("display", "flex"), ("flex-direction", "row"), ("flex-wrap", "nowrap")
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
        contents = model.buffer.contents
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
    div [ class "code-area"
        , style [ ("margin", "0"), ("padding", "0"), ("border", "none")
                , ("width", "100%")
                ]
        ]
        [ ruler (model.id ++ "-ruler")
        , cursorLayer model
        , markerLayer model
        , codeLayer model
        ]

codeLayer: Model  -> Html Msg
codeLayer model = 
    let
        contents = model.buffer.contents
        cursor = model.buffer.cursor
    in
        div [ id (model.id ++ "-codeLayer")
            , class "code-layer"
            , style [ ("margin", "0"), ("padding", "0"), ("border", "none")
                    , ("width", "100%")
                    ]
            ] <|
            List.indexedMap
                (λ n ln ->
                      if n == cursor.row then
                          div [ class "line"
                              , style [ ("height", "1em")
                                      , ("width", "100%")
                                      , ("text-wrap", "none")
                                      , ("white-space", "pre")
--                                      , ("display" , "inline-flex")
                                      ]
                              , onMouseDown (DragStart n)
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
                              , onMouseDown (DragStart n)
                              ] [text ln] ) contents

cursorLayer : Model -> Html Msg
cursorLayer model =
    {- 作戦2, [パディング用要素、本物の文字を入れ、hiddenにする][cursol]
       思いついたのでやってみたらうまく行った
       けど、みんなこれをやってないの、なにか落とし穴あるからなのかな？
    -}
    div [ class "cursor-layer"
        , style [("position", "absolute")]
        ]
        [ div [style [ ("position", "relative")
                     , ("display" , "inline-flex")
                     , ("flex-direction", "row")
                     , ("flex-wrap", "nowrap")
                     , ("justify-content", "flex-start")
                     , ("height", "1em")

                     , ("top" , (model.buffer.cursor.row |> toString) ++ "em")
                     , ("left", "0")
                     ]
               ]
               [ pad model
               , div
                     [ style [("position", "relative"), ("display" , "inline-flex")] ]
                     [ textarea [ id <| model.id ++ "-input"
                                , onInput Input
                                , onKeyDown KeyDown
                                , onCompositionStart CompositionStart
                                , onCompositionUpdate CompositionUpdate
                                , onCompositionEnd CompositionEnd
                                , value model.input_buffer
                                , style [ ("width", "1px"), ("border", "none"), ("padding", "0"), ("margin","0"), ("outline", "none")
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

markerLayer: Model -> Html Msg
markerLayer model =
    case model.buffer.selection of
        Nothing ->
            text ""

        Just sel ->
            let
                bpos = if (Buffer.isPreviosPos sel.begin sel.end) then sel.begin else sel.end
                epos = if (Buffer.isPreviosPos sel.begin sel.end) then sel.end else sel.begin

                ms = List.range (Tuple.first bpos) (Tuple.first epos)
                   |> List.map (\ r ->
                                    let
                                        cb = if r == (Tuple.first bpos)
                                             then bpos |> Tuple.second
                                             else 0
                                        ce = if r == (Tuple.first epos)
                                             then epos |> Tuple.second
                                             else String.length <| (Buffer.line r model.buffer.contents |> Maybe.withDefault "")
                                    in
                                        {row =r, begin_col = cb, end_col = ce}
                               )
            in
                div [ class "marker-layer"
                    , style [("position", "absolute")]
                    ]
                    ( List.map (\ m ->
                                  div [ style [ ("position", "absolute")
                                              , ("display", "inline-flex")
                                              , ("top" , (m.row |> toString) ++ "em")
                                              , ("left", "0")
                                              ]
                                      ]
                                      [ padToCursor (m.row, m.begin_col) model
                                      , div [ class "selection"
                                            , style [ ("background-color", "blue")
                                                    , ("color","white")
                                                    , ("white-space", "pre")
                                                    , ("border","none"), ("padding", "0"), ("margin", "0")
                                                    , ("z-index", "99") -- note: 範囲選択を一番上にしたいため 99 という数字自体に意味はない
                                                    ]
                                            ]
                                            [ Buffer.line m.row model.buffer.contents |> Maybe.withDefault ""
                                              |> String.dropLeft m.begin_col
                                              |> String.left (m.end_col - m.begin_col)
                                              |> text
                                            ]
                                      ]
                             ) ms )

pad : Model -> Html msg
pad model =
    let
        cur      = model.buffer.cursor
        contents = model.buffer.contents
    in
    span [ class "pad"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ Buffer.line cur.row contents |> Maybe.withDefault "" |> String.left cur.column |> text ]

padToCursor : (Int, Int) -> Model -> Html msg
padToCursor pos model =
    let
        contents = model.buffer.contents
    in
    span [ class "pad"
         , style [ ("position", "relative")
                 , ("white-space", "pre")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ Buffer.line (Tuple.first pos) contents |> Maybe.withDefault "" |> String.left (Tuple.second pos) |> text ]



ruler : String -> Html msg
ruler base_id = 
    span [ id base_id
         , style [ ("position", "absolute")
                 , ("white-space", "pre")
                 , ("color", "green")
                 ]
         ]
         []

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
         , id <| model.id ++ "-cursor"
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

-- Mouse Event

onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Json.map tagger Mouse.position)


------------------------------------------------------------
-- Native (Mice)
------------------------------------------------------------

-- Task

doFocus : String -> Task Never Bool
doFocus id =
    Task.succeed (Native.Mice.doFocus id)

copyToClipboard : String -> String -> Task Never Bool
copyToClipboard id text =
    Task.succeed (Native.Mice.copyToClipboard id text)

elaborateInputAreaEventHandlers: String -> String -> Task Never Bool
elaborateInputAreaEventHandlers input_area_id paste_area_id =
    Task.succeed (Native.Mice.elaborateInputAreaEventHandlers input_area_id paste_area_id)

setScrollTop : String -> Int -> Task Never Bool
setScrollTop id pixels =
    Task.succeed (Native.Mice.setScrollTop id pixels)

setScrollLeft : String -> Int -> Task Never Bool
setScrollLeft id pixels =
    Task.succeed (Native.Mice.setScrollLeft id pixels)


-- Function

calcTextWidth : String -> String -> Int
calcTextWidth id txt = Native.Mice.calcTextWidth id txt

type alias Rect =
    { left :Int
    , top : Int
    , right: Int
    , bottom : Int
    , x : Int
    , y : Int
    , width :Int
    , height : Int
    }

getBoundingClientRect: String -> Rect
getBoundingClientRect id = Native.Mice.getBoundingClientRect id

getScrollTop: String -> Int
getScrollTop id = Native.Mice.getScrollTop id

getScrollLeft: String -> Int
getScrollLeft id = Native.Mice.getScrollLeft id

getScrollHeight : String -> Int
getScrollHeight id = Native.Mice.getScrollHeight

