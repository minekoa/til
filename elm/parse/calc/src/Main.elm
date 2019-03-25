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
        [ map (\_ -> Minus) (keyword "-")
        , map (\_ -> Plus) (keyword "+")
        ]


type UnOperator = UnMinus | UnPlus
type BinOperator = Minus | Plus

type CalcAST
    =  Number Float
    | BinOperation CalcAST BinOperator CalcAST
    | UnOperation  UnOperator CalcAST
    | Paren CalcAST


term : Parser CalcAST
term =
    -- 単項
    number
--    oneOf
--        [ number
--        , paren
--        ]


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

binOperation : Parser CalcAST
binOperation = 
    loop (Number 0) binOperationHelper

--    succeed (\lhs rhs -> BinOperation lhs Plus rhs)
--        |= term
--        |= number
--        |= loop (Number 0) binOperationHelper

binOperationHelper lhs =
    oneOf
        [ succeed (\op rhs -> Loop (BinOperation lhs op rhs))
            |= binOperator
--            |= term
            |= number
        , succeed ()
            |> map (\_ -> Done lhs)
        ]

paren =
    succeed Paren
        |. symbol "("
        |= expr
        |. symbol ")"


parse source =
--    Parser.run paren source
    Parser.run binOperation source
--    Parser.run expr source

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
