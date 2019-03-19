module KmlParser exposing (State, parse)

--Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore)

import Parser exposing (..)
import Parser.Advanced exposing (Token, chompUntil)
import Set


parse source =
    Parser.run state source



-- State ::= "state" TypeName "@" "(-" CommentString "-)" "{" StateBody "}:


type alias State =
    { name : String
    , naturalLang : String
    , body : List Transition
    }


type alias Transition =
    { event : String
    , guird : Maybe Guird
    , post : PostCondition
    }


type alias PostCondition =
    { expression : String
    , naturalLang : String
    }

type alias Guird =
--    { expression : Expression
    { expression : String
    , naturalLang : String
    }

state : Parser State
state =
    -- State ::= "state" TypeName "@" "(-" CommentString "-)" "{" StateBody "}:
    succeed State
        |. keyword "state"
        |. spaces
        |= stateName
        |. spaces
        |. symbol "@"
        |. spaces
        |= naturalLangSpec "(-" "-)"
        --NotNestable
        |. spaces
        |= stateBody


stateName =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }


stateBody : Parser (List Transition)
stateBody =
    succeed identity
        |. spaces
        |. symbol "{"
        |. spaces
        |= prosessExpressions
        |. spaces
        |. symbol "}"



--Transition ::= "transition" EventExpression (Guird)? "-->" (TransitionBody | InternalChoiceList)


prosessExpressions : Parser (List Transition)
prosessExpressions =
    succeed (::)
        |= prosessExpression
        |= prosessExpressionTail


prosessExpressionTail : Parser (List Transition)
prosessExpressionTail =
    oneOf
        [ succeed (::)
            |= prosessExpression
            |= lazy (\_ -> prosessExpressionTail)
        , succeed []
        ]


prosessExpression : Parser Transition
prosessExpression =
    transition

type alias Expression =
    { name : String
    , params : ParameterList
    }

type ParameterList = ParameterList (List Expression)

expression =
    succeed identity
        |= oneOf
        [ functionCall
--        , binOperation
        ]

functionCall : Parser Expression
functionCall =
    succeed (\n prms -> Expression n (ParameterList prms))
        |= varName
        |= lazy (\_ -> expressionList)

-- binOperation =
--     succeed (\ lhs op rhs -> Expression op (ParameterList [lhs, rhs]))
--         |= expression
--         |= binOperator
--         |= lazy (\_ -> expression)

-- binOperator =
--     succeed identity
--         |= oneOf
--             [ symbol "+"
--             , symbol "-"
--             , symbol "*"
--             , symbol "/"
--             , symbol "%"
--             , symbol "&&"
--             , symbol "||"
--             , symbol "=="
--             , symbol "/="
--             ]

varName =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }

varTypeName =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }

expressionList : Parser (List Expression)
expressionList =
    succeed (::)
        |= expression
        |. spaces
        |= expressionListTail

expressionListTail : Parser (List Expression)
expressionListTail =
    oneOf
        [ succeed (::)
            |= expression
            |. spaces
            |= lazy (\_ -> expressionListTail)
        , succeed []
        ]

transition : Parser Transition
transition =
    succeed Transition
        |. keyword "transition"
        |. spaces
        |= eventExpression
        |. spaces
        |= maybeGuird
        |. symbol "-->"
        |. spaces
        |= transitionBody
        |. spaces

maybeGuird =
    oneOf
    [ succeed Just
        |= guirdClause
        |. spaces
    , succeed Nothing
    ]

guirdClause =
    succeed Guird
        |. keyword "when"
        |. spaces
--        |. symbol "[" --fordebug
--        |. spaces     --fordebug
        |= guirdExpression
--        |= expression
--        |. spaces     --fordebug
--        |. symbol "]" --fordebug
        |. spaces
        |. symbol "@"
        |. spaces
        |= naturalLangSpec "[-" "-]"

guirdExpression =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }


transitionBody : Parser PostCondition
transitionBody =
    succeed identity
        |. symbol "{"
        |. spaces
        |= postCondition
        |. spaces
        |. symbol "}"
        |. spaces


postCondition =
    -- post {
    --     <<Post Expression>>
    -- } @ {-
    --     <<Natural Language Spec>>
    -- }
    succeed PostCondition
        |. keyword "post"
        |. spaces
        |. symbol "{"
        |. spaces
        |= postExpression
        |. spaces
        |. symbol "}"
        |. spaces
        |. symbol "@"
        |. spaces
        |= naturalLangSpec "{-" "-}"


postExpression =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }


eventExpression =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "forall" ]
        }


naturalLangSpec : String -> String -> Parser String
naturalLangSpec open close =
    succeed (\s -> s)
        |. token open
        |= stateName
        |. token close


toToken : String -> Parser.Advanced.Token Problem
toToken str =
    Parser.Advanced.Token str (Expecting str)
