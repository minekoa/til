module Editor.Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

{-| This module is simple texteditor.

-}

type alias Cursor =
    { row : Int
    , column : Int
    }


type alias Model =
    { cursor : Cursor
    , contents : List String
    , history : Maybe String
    }

init : String -> Model
init text =
    Model (Cursor 0 0)
          (String.lines text)
          Nothing


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



------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html msg
view model =
    div [class "editor"]
        [ presentation model ]

presentation : Model -> Html msg
presentation model =
    div [ style [ ("display", "flex"), ("flex-direction", "row"), ("flex-wrap", "no-wrap")
                , ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                ]
        ]
        [ lineNumArea model
        , codeArea model
        ]

lineNumArea : Model -> Html msg
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

codeArea : Model -> Html msg
codeArea model =
    div [ class "code-area" ]
        [ --cursorLayer model.cursor
          cursorLayer2 model
        , codeLayer model.contents
--        , ruler model
        ]

codeLayer: List String  -> Html msg
codeLayer contents = 
    div [class "lines"] <|
        List.map (λ ln -> div [ class "line"
                               , style [("height", "1em")
                                       ,("text-wrap", "none")]
                               ] [text ln] ) contents


cursorLayer : Cursor -> Html msg
cursorLayer cur =
    {- 作戦1, ちゃんと場所を計算する
       rulerはつくったが、DOMから取ってくるのがめんどい。未だ作ってない
    -}
    div [ class "cursors" 
        , style [("position", "absolute")]
        ]
        [ div [ class "cursor"
              , style [ ("position", "relative")

                      , ("background-color", "red")
                      , ("opacity", "0.5")

                      , ("height", "1em")
                      , ("width" , "0.5em")

                      , ("top" , (cur.row |> toString) ++ "em")
                      , ("left", (cur.column |> toString) ++ "em")


                      , ("z-index", "1")
                      ]
              ]
              []
        ]

cursorLayer2 : Model -> Html msg
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
               , span [style [ ("background-color", "blue")
                             , ("opacity", "0.5")
                             , ("height", "1em")
                             , ("width", "3px")
                             ]
                      ]
                      []
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
                 , ("white-space", "nowrap")
                 , ("visibility", "hidden")                     
                 ]
         ]
         [ line cur.row contents |> Maybe.withDefault "" |> String.left cur.column |> text]


