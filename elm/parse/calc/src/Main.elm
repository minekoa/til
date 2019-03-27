module Main exposing (Model, Msg(..), init, main, view)

import Browser exposing (..)
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, style, value)
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


type UnOperator
    = Positive
    | Negative


type BinOperator
    = Minus
    | Plus


unOperator : Parser UnOperator
unOperator =
    oneOf
        [ map (\_ -> Positive) (keyword "+")
        , map (\_ -> Negative) (keyword "-")
        ]


binOperator : Parser BinOperator
binOperator =
    oneOf
        [ map (\_ -> Minus) (keyword "-")
        , map (\_ -> Plus) (keyword "+")
        ]


type CalcAST
    = Number Float
    | BinOperation CalcAST BinOperator CalcAST
    | UnOperation UnOperator CalcAST
    | Paren CalcAST


program : Parser CalcAST
program =
    succeed identity
        |= expr
        |. end


expr : Parser CalcAST
expr =
    -- 多項
    oneOf
        [ binOperation
        , lazy (\_ -> term)
        ]


term : Parser CalcAST
term =
    -- 単項
    oneOf
        [ number
        , paren
        , unnayOperation
        ]


number : Parser CalcAST
number =
    succeed Number
        |. symbol "'"
        |= float


unnayOperation : Parser CalcAST
unnayOperation =
    succeed UnOperation
        |= unOperator
        |= lazy (\_ -> term)


binOperation : Parser CalcAST
binOperation =
    lazy (\_ -> term)
        |> andThen (\lhs -> loop lhs binOperationHelper)


binOperationHelper : CalcAST -> Parser (Step CalcAST CalcAST)
binOperationHelper lhs =
    oneOf
        [ succeed (\op rhs -> Loop (BinOperation lhs op rhs))
            |= binOperator
            |= lazy (\_ -> term)
        , succeed (Done lhs)
        ]


paren : Parser CalcAST
paren =
    succeed Paren
        |. symbol "("
        |= expr
        |. symbol ")"


parse source =
    Parser.run program source



------------------------------------------------------------


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
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
        , div []
            [ case model.parseResult of
                Ok ast ->
                    viewAST ast

                Err e ->
                    text ""
            ]
        ]


astAttribute : List (Html.Attribute Msg)
astAttribute =
    [ class "ast"
    , style "border" "1px dotted green"
    , style "padding" "0.5em"
    ]


viewAST : CalcAST -> Html Msg
viewAST ast =
    case ast of
        Number f ->
            div astAttribute [ text <| String.fromFloat f ]

        BinOperation rhs op lhs ->
            div astAttribute
                [ viewAST rhs
                , viewBinOperator op
                , viewAST lhs
                ]

        UnOperation op v ->
            div astAttribute
                [ viewUnOperator op
                , viewAST v
                ]

        Paren v ->
            viewAST v


viewBinOperator : BinOperator -> Html Msg
viewBinOperator op =
    case op of
        Minus ->
            text "(-)"

        Plus ->
            text "(+)"


viewUnOperator : UnOperator -> Html Msg
viewUnOperator op =
    case op of
        Positive ->
            text "+"

        Negative ->
            text "-"
