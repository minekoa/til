module KmlParser exposing (parse, State)

import Parser exposing (..)--Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore)
import Parser.Advanced exposing(Token, chompUntil)
import Set

parse source =
    Parser.run state source


-- State ::= "state" TypeName "@" "(-" CommentString "-)" "{" StateBody "}:

type alias State =
    { name: String
    , comment : String
    , body : String
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
        |= comment "(-" "-)" --NotNestable
        |. spaces
        |= comment "{" "}" --NotNestable --暫定

stateName =
    variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList ["forall"]
    }

comment : String -> String -> Parser String
comment open close =
    succeed (\s -> s)
        |. token open
        |= stateName
        |. token close

toToken : String -> Parser.Advanced.Token Problem
toToken str =
  Parser.Advanced.Token str (Expecting str)
