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
    head (drop n lines)

------------------------------------------------------------
-- cursor
------------------------------------------------------------

moveForward : Model -> Model
moveForward model =
    let
        cur = model.cursor
        ln  = model.contents !! cur.row
    in
       if cur.column < Line.length ln


------------------------------------------------------------
-- View
------------------------------------------------------------

view : Model -> Html msg
view model =
    div [class "editor"]
        [ codeLayer model.contents ]

codeLayer: List String  -> Html msg
codeLayer contents = 
    div [ style [ ("display", "flex"), ("flex-direction", "row"), ("flex-wrap", "no-wrap")
                , ("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")
                ]
        ]
        [ div [ class "line-num-colmn"
              , style [ ("text-align", "right")
                      , ("padding-right", "0.2em")]
              ] <|
              List.map
                  (λ n -> div [ class "line-num"
                              , style [ ("height", "1em")
                                      , ("text-wrap", "none")]
                              ] [ text (toString n) ])
                  (List.range 1 (List.length contents))
        , div [class "line-colmn"] <|
              List.map (λ ln -> div [ class "line"
                                    , style [("height", "1em")
                                            ,("text-wrap", "none")]
                                    ] [text ln] ) contents
        ]

