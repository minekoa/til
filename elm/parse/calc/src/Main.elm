module Main exposing (Model, Msg(..), init, main, view)

import Browser exposing (..)
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



import Parser exposing (..)
import Parser.Advanced exposing (Token, chompUntil)

-- expression ::= term
--            |   expression '+' term
--            |   expression '-' term

-- term ::= block
--       | term '*' block
--       | term '/' block

-- block ::= '(' expression ')'
--        | integer

unOperator =
    oneOf
        [ map (\_ -> UnMinus) (keyword "-")
        , map (\_ -> UnPlus) (keyword "+")
        ]

binOperator =
    oneOf
        [ map (\_ -> BinMinus) (keyword "-")
        , map (\_ -> BinPlus) (keyword "+")
        ]


type UnOperator = UnMinus | UnPlus
type BinOperator = BinMinus | BinPlus

type CalcAST
    =  Number Float
    | BinOperation BinOperator CalcAST CalcAST
    | UnOperation  UnOperator CalcAST
    | Paren CalcAST


term : Parser CalcAST
term =
    -- 単項
    oneOf
        [ paren
        , number
        ]
expr =
    -- 多項
    oneOf
        [ binOperation
        , lazy (\_ -> term)
        ]

number =
    succeed Number
        |= float

unnayOperation =
    succeed UnOperation
        |= unOperator
        |= term

binOperation =
    succeed (\ t1 op t2 -> BinOperation op t1 t2)
        |= lazy (\_ -> term)
        |= binOperator
        |= lazy (\_ -> expr)

paren =
    succeed Paren
        |. symbol "("
        |= expr
        |. symbol ")"


parse source =
    Parser.run expr source

------------------------------------------------------------


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Model  =
    { source : String
    , parseResult : Result (List Parser.DeadEnd) CalcAST
    , message : Maybe String
    }

init =
    Model "" (parse "") Nothing

type Msg
    = Input String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input s ->
            { model
                | source = s
                , parseResult = parse s
            }

view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput Input, value model.source ] []
        , div [] [ text model.source ]
        , div []
            [ case model.parseResult of
                Ok s ->
                    text <| Debug.toString s

                Err e ->
                    text <| Debug.toString e
            ]
        , div [] [ model.message |> Maybe.withDefault "" |> text ]
        ]
