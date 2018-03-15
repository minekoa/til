module Editor exposing ( Model
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
import Json.Encode
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
    , copyStore : String

    -- frame
    , enableComposer : Bool
    , compositionPreview : Maybe String --IMEで返還中の未確定文字
    , focus : Bool
    , blink : BlinkState

    -- for debug
    , event_log : Maybe (List String)
    }

init : String -> String -> (Model, Cmd Msg)
init id text =
    ( Model id                     -- id
            (Buffer.init text)
            ""                     -- copyStore
            False Nothing          -- COMPOSER STATE
            False                  -- focus
            BlinkBlocked           -- blink

            Nothing                -- event_log
    , Cmd.none
    )


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


------------------------------------------------------------
-- update
------------------------------------------------------------

type Msg
    = IgnoreResult
    | Pasted String
    | Copied String
    | Cutted String
    | Input String
    | KeyDown KeyboardEvent
    | KeyPress Int
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

        Pasted s ->
            paste s model
                |> Tuple.mapFirst (eventLog "pasted" s)

        Copied s ->
            copy model
                |> Tuple.mapFirst (eventLog "copied" s)

        Cutted s ->
            cut model
                |> Tuple.mapFirst (eventLog "cutted" s)


        -- View Operation Event

        Input s ->
            input s model

        KeyDown keyevent ->
            keyDown keyevent model
 
        KeyPress code ->
            keyPress code model

        CompositionStart data ->
            compositionStart data model

        CompositionUpdate data ->
            compositionUpdate data model

        CompositionEnd data ->
            compositionEnd data model

        FocusIn _ ->
            ( { model | focus = True }
            , Task.perform (\_ -> IgnoreResult) (elaborateInputArea (inputAreaID model)) )

        FocusOut _ ->
            ( { model|focus = False }
            , Cmd.none)

        SetFocus ->
            ( model
                |> eventLog "setfocus" ""
            , Task.perform (\_ -> IgnoreResult) (doFocus <| inputAreaID model) )

        DragStart row xy ->
            let
                calc_w  = calcTextWidth (rulerID model)
                calc_col = (\ ln c x ->
                              if (calc_w (String.left c ln)) > x || String.length ln < c  then c - 1
                              else calc_col ln (c + 1)  x)

                ln = Buffer.line row model.buffer.contents |> Maybe.withDefault ""
                rect = getBoundingClientRect (codeLayerID model)

                col = (calc_col ln 0 (xy.x - rect.left))

                b1 = model.buffer
                b2 = { b1 | cursor = Buffer.Cursor row col }


            in
                ( { model | buffer = b2 }
                  |> eventLog "dragstart" ("pos=" ++ (toString xy.x) ++ "," ++ (toString xy.y)
                                               ++ "; offsetx=" ++ (toString (xy.x - rect.left))
                                               ++ "; row=" ++ (toString row)
                                               ++ "; calced_col=" ++ (toString col)
                                          )
                  |> blinkBlock
                , Task.perform (\_ -> IgnoreResult) (doFocus <| inputAreaID model) -- firefox 限定で、たまーに、SetFocus が来ないことがあるので、ここでもやっとく。
                )

        Tick new_time ->
            ( {model | blink = blinkTransition model.blink }
            , Cmd.none )



input: String -> Model -> (Model, Cmd Msg)
input s model =
    case model.enableComposer of
        True ->
            ( model
              |> eventLog "input (ignored)" s
            , Cmd.none )
        False ->
            insert (String.right 1 s) model
                |> Tuple.mapFirst (eventLog "input" (String.right 1 s))


keymapper : (Bool, Bool, Bool, Int) -> (Model -> (Model, Cmd Msg))
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

                 , {ctrl=False, alt=False, shift=False, code=  8, f=backspace } -- BS
                 , {ctrl=False, alt=False, shift=False, code= 46, f=delete }    -- DEL

                 -- note: C-c, C-x, C-v は、システムのクリップボードと連携したいので、
                 --       ブラウザの ClipboardEvent (copy, cut, paste)を発火させるため、ここでは何もしない

                 , {ctrl=True , alt=False, shift=False, code= 90, f= undo }

                 -- emacs like binds
                 , {ctrl=True , alt=False, shift=False, code= 70, f=moveForward }  -- 'C-f'
                 , {ctrl=True , alt=False, shift=False, code= 66, f=moveBackward } -- 'C-b'
                 , {ctrl=True , alt=False, shift=False, code= 78, f=moveNext }     -- 'C-n'
                 , {ctrl=True , alt=False, shift=False, code= 80, f=movePrevios }  -- 'C-p'
                 , {ctrl=True , alt=False, shift=False, code= 72, f=backspace }    -- 'C-h'
                 , {ctrl=True , alt=False, shift=False, code= 68, f=delete }       -- 'C-d'
                 , {ctrl=False , alt=True, shift=False, code= 87, f=copy }         -- 'M-w' (注: クリップボード連携なし)
                 , {ctrl=True , alt=False, shift=False, code= 87, f=cut  }         -- 'C-w' (注: クリップボード連携なし)
                 , {ctrl=True , alt=False, shift=False, code= 77, f= insert "\n" } -- 'C-m'
                 , {ctrl=True , alt=False, shift=False, code= 89, f=(\m -> paste m.copyStore m) } -- 'C-y'
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
            |> Maybe.withDefault (\m -> (m, Cmd.none) )
                                  

keyDown : KeyboardEvent -> Model -> (Model, Cmd Msg)
keyDown e model =
    keymapper (e.ctrlKey, e.altKey, e.shiftKey, e.keyCode) model
        |> Tuple.mapFirst (eventLog "keydown" (keyboarEvent_toString e))

keyPress : Int -> Model -> (Model, Cmd Msg)
keyPress code model =
    -- IME入力中にkeypress イベントがこないことを利用して IME入力モード(inputを反映するか否かのフラグ）を解除
    -- ※ compositonEnd で解除してしまうと、firefoxとchromeの振る舞いの違いでハマる
    --        chrome  :: keydown 229 -> compositionend s
    --        firefox ::   (null)    -> compositionend s -> input s
    ( model
        |> composerDisable
        |> eventLog "keypress" (toString code)
    , Cmd.none
    )

compositionStart : String -> Model -> (Model, Cmd Msg)
compositionStart data model =
    -- note: この data は入力された文字列 **ではない**
    ( { model
          | buffer = buffer_delete_selection model.buffer
          , compositionPreview = Just ""
      }
      |> composerEnable ""
      |> blinkBlock
      |> eventLog "compositoinstart" data
    , Cmd.none
    )

compositionUpdate : String -> Model -> (Model, Cmd Msg)
compositionUpdate data model =
    ( { model | compositionPreview = Just data }
      |> blinkBlock
      |> eventLog "compositionupdate" data
    , Cmd.none
    )

compositionEnd : String -> Model -> (Model, Cmd Msg)
compositionEnd data model =
    -- note: 変換プレビューのクリアはするが、
    --        firefox ではこの後 input イベントがくるので、
    --        それを無視する為 enable-conposerは立てたままにする (keypressイベントで解除する、そちらを参照)
    { model
        | buffer = buffer_insert data model.buffer
        , compositionPreview = Nothing
    }
        |> blinkBlock
        |> eventLog "compositionend" data
        |> withEnsureVisibleCmd

------------------------------------------------------------
-- control state update
------------------------------------------------------------

eventLog : String -> String -> Model -> Model
eventLog ev data model =
    let
        s = "(" ++ ev ++ ":" ++ data ++ ") "
    in
        { model | event_log = Maybe.andThen (\logs -> Just (s :: logs)) model.event_log }

composerEnable : String -> Model -> Model
composerEnable data model =
    { model
        | compositionPreview = Just data
        , enableComposer = True
    }

composerDisable : Model -> Model
composerDisable model =
    { model
        | compositionPreview = Nothing
        , enableComposer = False
    }

------------------------------------------------------------
-- scroll command generator
------------------------------------------------------------

withEnsureVisibleCmd : Model -> (Model, Cmd Msg)
withEnsureVisibleCmd model =
    ( model
    , ensureVisible model (\_ -> IgnoreResult)
    )

ensureVisible : Model -> (Bool -> msg) -> Cmd msg
ensureVisible model tagger =
    Cmd.batch
        [ calcVScrollPos model
              |> Maybe.andThen
                  (\n -> Just <| Task.perform tagger (setScrollTop (frameID model) n))
              |> Maybe.withDefault Cmd.none
        , calcHScrollPos model
              |> Maybe.andThen
                  (\n -> Just <| Task.perform tagger (setScrollLeft (frameID model) n))
              |> Maybe.withDefault Cmd.none
        ]

calcVScrollPos : Model -> Maybe Int
calcVScrollPos model =
    let
        frameRect  = getBoundingClientRect <| frameID model
        cursorRect = getBoundingClientRect <| cursorID model
        scrtop = getScrollTop (frameID model)

        margin = cursorRect.height * 3
    in
        if  cursorRect.top - margin < frameRect.top then
            Just ( scrtop + (cursorRect.top - frameRect.top ) - margin)
        else
            if  cursorRect.bottom + margin > frameRect.bottom then
                Just ( scrtop + (cursorRect.bottom - frameRect.bottom ) + margin)
            else 
                Nothing

calcHScrollPos : Model -> Maybe Int
calcHScrollPos model =
    let
        frameRect  = getBoundingClientRect <| frameID model
        cursorRect = getBoundingClientRect <| cursorID model
        scrleft    = getScrollLeft (frameID model)

        margin = cursorRect.height * 3
    in
        if cursorRect.left - margin < frameRect.left then
            Just ( scrleft + (cursorRect.left - frameRect.left ) - margin)
        else
            if  cursorRect.right + margin > frameRect.right then
                Just ( scrleft + (cursorRect.right - frameRect.right ) + margin)
            else 
                Nothing


------------------------------------------------------------
-- update > cursor
------------------------------------------------------------

-- Tools

moveF : (Buffer.Model -> Buffer.Model) -> Model -> (Model, Cmd Msg)
moveF f model =
    { model | buffer = f model.buffer }
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd

-- API

moveForward : Model -> (Model, Cmd Msg)
moveForward = moveF Buffer.moveForward

moveBackward : Model -> (Model, Cmd Msg)
moveBackward = moveF Buffer.moveBackward

movePrevios : Model -> (Model, Cmd Msg)
movePrevios = moveF Buffer.movePrevios

moveNext : Model -> (Model, Cmd Msg)
moveNext = moveF Buffer.moveNext


------------------------------------------------------------
-- update > selection
------------------------------------------------------------

-- Tools

selectionClear : Model -> Model
selectionClear model =
    { model | buffer = Buffer.selectionClear model.buffer }

selectF : (Buffer.Model -> Buffer.Model) -> Model -> (Model, Cmd Msg)
selectF f model =
    { model | buffer = f model.buffer }
        |> blinkBlock
        |> withEnsureVisibleCmd

-- API

selectBackward: Model -> (Model, Cmd Msg)
selectBackward = selectF Buffer.selectBackward

selectForward: Model -> (Model, Cmd Msg)
selectForward = selectF Buffer.selectForward

selectPrevios: Model -> (Model, Cmd Msg)
selectPrevios = selectF Buffer.selectPrevios

selectNext: Model -> (Model, Cmd Msg)
selectNext = selectF Buffer.selectNext


------------------------------------------------------------
-- update > edit
------------------------------------------------------------

-- Tools

editF : (Buffer.Model -> Buffer.Model) -> Model -> (Model, Cmd Msg)
editF f model =
    { model | buffer = f model.buffer }
        |> blinkBlock
        |> withEnsureVisibleCmd

buffer_insert : String -> Buffer.Model -> Buffer.Model
buffer_insert text bufmodel=
    case bufmodel.selection of
        Nothing ->
            Buffer.insert (Buffer.nowCursorPos bufmodel) text bufmodel
        Just s ->
            bufmodel
                |> Buffer.deleteRange s
                |> Buffer.selectionClear
                |> (\m -> Buffer.insert (Buffer.nowCursorPos m) text m)

buffer_backspace : Buffer.Model -> Buffer.Model
buffer_backspace bufmodel =
    case bufmodel.selection of
        Nothing ->
            Buffer.backspace (Buffer.nowCursorPos bufmodel) bufmodel
        Just s ->
            bufmodel
                |> Buffer.deleteRange s
                |> Buffer.selectionClear

buffer_delete : Buffer.Model -> Buffer.Model
buffer_delete bufmodel =
    case bufmodel.selection of
        Nothing ->
            Buffer.delete (Buffer.nowCursorPos bufmodel) bufmodel
        Just s ->
            bufmodel
                |> Buffer.deleteRange s
                |> Buffer.selectionClear

buffer_delete_selection : Buffer.Model -> Buffer.Model
buffer_delete_selection bufmodel =
    case bufmodel.selection of
        Nothing ->
            bufmodel
        Just s  ->
            bufmodel
                |> Buffer.deleteRange s
                |> Buffer.selectionClear

-- API

insert: String -> Model-> (Model, Cmd Msg)
insert text = editF (buffer_insert text)

backspace: Model -> (Model, Cmd Msg)
backspace = editF buffer_backspace

delete: Model ->  (Model, Cmd Msg)
delete = editF buffer_delete


-- API (Extra: 場所指定して編集)

insertPos: (Int, Int) -> String -> Model -> (Model, Cmd Msg)
insertPos (row, col) text  model =
    { model | buffer = Buffer.insert (row, col) text model.buffer }
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd

deletePos: (Int, Int) -> Model -> (Model, Cmd Msg)
deletePos (row, col) model =
    { model | buffer = Buffer.delete (row, col) model.buffer}
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd

deleteRange: Buffer.Range -> Model -> (Model, Cmd Msg)
deleteRange selection model =
    { model | buffer = Buffer.deleteRange selection model.buffer}
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd


------------------------------------------------------------
-- update > undo / redo
------------------------------------------------------------

-- API

undo : Model -> (Model, Cmd Msg)
undo model =
    { model | buffer = Buffer.undo model.buffer }
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd


------------------------------------------------------------
-- update > clipboard action
------------------------------------------------------------

-- API (for User and Browser's clipboard action (Copied, Cutted, Pasted (custom events)) )

copy : Model -> (Model, Cmd Msg)
copy model =
    -- note: ブラウザのセキュリティ制約により、sytem の clipboard  にはコピーされません
    ( case model.buffer.selection of
          Nothing -> model
          Just sel ->
          { model
              | copyStore = Buffer.readRange sel model.buffer
              , buffer = Buffer.selectionClear model.buffer
          }
    )
        |> blinkBlock
        |> (\m -> (m, Cmd.none))


cut : Model -> (Model, Cmd Msg)
cut model =
    -- note: ブラウザのセキュリティ制約により、sytem の clipboard  にはコピーされません
    ( case model.buffer.selection of
          Nothing -> model
          Just sel ->
          { model
              | copyStore = Buffer.readRange sel model.buffer
              , buffer = model.buffer |> Buffer.deleteRange sel |> Buffer.selectionClear
          }
    )
        |> blinkBlock
        |> withEnsureVisibleCmd

paste : String -> Model -> (Model, Cmd Msg)
paste text model =
    { model
        | buffer = model.buffer
                       |> Buffer.insert (Buffer.nowCursorPos model.buffer) text
                       |> Buffer.selectionClear
        , copyStore = text  -- clipboard経由のペーストもあるので、copyStoreを更新しておく
    }
        |> blinkBlock
        |> withEnsureVisibleCmd



-- API (Extra: 場所を指定して編集)

copyRange : Model -> Buffer.Range -> (Model, Cmd Msg)
copyRange model selection =
    -- note: ブラウザのセキュリティ制約により、sytem の clipboard  にはコピーされません
    let
        s = Buffer.readRange selection model.buffer
    in
        { model | copyStore = s }
            |> selectionClear
            |> blinkBlock
            |> (\m -> (m, Cmd.none))

cutRange : Model -> Buffer.Range -> (Model, Cmd Msg)
cutRange model selection =
    -- note: ブラウザのセキュリティ制約により、sytem の clipboard  にはコピーされません
    let
        s = Buffer.readRange selection model.buffer
    in
        { model | copyStore = s
                , buffer = Buffer.deleteRange selection model.buffer
        }
        |> selectionClear
        |> blinkBlock
        |> withEnsureVisibleCmd

pastePos : Model -> (Int, Int) -> String -> (Model, Cmd Msg)
pastePos model (row, col) text =
    { model | buffer = Buffer.insert (row, col) text model.buffer 
            , copyStore = text  -- clipboard経由のペーストもあるので、copyStoreを更新しておく
    }
    |> selectionClear
    |> blinkBlock
    |> withEnsureVisibleCmd


------------------------------------------------------------
-- id gen
------------------------------------------------------------

frameID : Model -> String
frameID model =
    model.id ++ "-editor-frame"

sceneID : Model -> String
sceneID model =
    model.id ++ "-editor-scene"

codeLayerID : Model -> String
codeLayerID model =
    model.id ++ "-editor-codeLayer"

rulerID : Model -> String
rulerID model =
    model.id ++ "-editor-ruler"

cursorID : Model -> String
cursorID model =
    model.id ++ "-editor-cursor"

inputAreaID : Model -> String
inputAreaID model =
    model.id ++ "-editor-input"


------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ id <| frameID model
        , style [ ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                , ("overflow","auto")
                , ( "position", "relative")
                , ("user-select", "none")
                , ("-webkit-user-select", "none")
                , ("-moz-user-select", "none")
                ]
        ]
        [ div [ id <| sceneID model
              , class "editor-scene"
              , style [ ( "position", "relative") ]
              ]
              [ presentation model 
              ]
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
        [ ruler <| rulerID model
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
        div [ id <| codeLayerID model
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
                                      ]
                              , onMouseDown (DragStart n)
                              ]
                              [ span [ style [ ("position", "relative")
                                             , ("white-space", "pre")
                                             ]
                                     ]
                                    [ text <| String.left cursor.column ln]
                              , compositionPreview model.compositionPreview
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
                     [ textarea [ id <| inputAreaID model
                                , onInput Input
                                , onKeyDown KeyDown
                                , onKeyPress KeyPress
                                , onCompositionStart CompositionStart
                                , onCompositionUpdate CompositionUpdate
                                , onCompositionEnd CompositionEnd
                                , onPasted Pasted
                                , onCopied Copied
                                , onCutted Cutted
                                , selecteddata <| Buffer.selectedString model.buffer
                                , spellcheck False
                                , wrap "off"
                                , style [ ("border", "none"), ("padding", "0"), ("margin","0"), ("outline", "none")
                                        , ("overflow", "hidden"), ("opacity", "0")
                                        , ("resize", "none")
                                        , ("position", "absolute")
                                        ]
                                ]
                           []
                     , span [ style [("visibility", "hidden") ]] [compositionPreview model.compositionPreview]
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
                                              |> (\l -> if l == "" then " " else l)
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
         , id <| cursorID model
         ]
    []


------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch [ Time.every (0.5 * second) Tick ]


------------------------------------------------------------
-- html events / attributes (extra)
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


-- CustomEvent (clipboard)

onPasted: (String -> msg) -> Attribute msg
onPasted tagger =
    on "pasted" (Json.map tagger (Json.field "detail" Json.string))

onCopied: (String -> msg) -> Attribute msg
onCopied tagger =
    on "copied" (Json.map tagger (Json.field "detail" Json.string))

onCutted: (String -> msg) -> Attribute msg
onCutted tagger =
    on "cutted" (Json.map tagger (Json.field "detail" Json.string))


-- CustomAttributes

selecteddata : Maybe String -> Attribute msg
selecteddata selected_str =
    selected_str
        |> Maybe.withDefault ""
        |> Json.Encode.string
        |> property "selecteddata"


------------------------------------------------------------
-- Native (Mice)
------------------------------------------------------------

-- Task

doFocus : String -> Task Never Bool
doFocus id =
    Task.succeed (Native.Mice.doFocus id)

elaborateInputArea: String  -> Task Never Bool
elaborateInputArea input_area_id =
    Task.succeed (Native.Mice.elaborateInputArea input_area_id)

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

