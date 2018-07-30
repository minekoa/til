module HogeLangParser exposing (..)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import String
import Basics

{-|


<Expression>  := <Bool>
               | <Int>
               | <String>
               | <Expression> "&&" <Expression>
               | <Expression> "||" <Expression>

<Bool> := true | false
<Int>  := (integer)
<String>  := "(string)"
-}


type Expression
    = EBool Bool
    | EInt Int
    | EString String
    | EIdentifier String
    | EAnd Expression Expression
    | EOr Expression Expression
    | ENot Expression
    | EAdd Expression Expression
    | ESub Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression





bool : Parser s Expression
bool =
    map EBool
        (choice
            [ False <$ string "false"
            , True <$ string "true"
            ]
        )
        |> mapError (Basics.always [ "boolean" ])

int : Parser s Expression
int =
    map EInt Combine.Num.int
        |> mapError (Basics.always [ "integer" ])

str : Parser s Expression
str =
    map EString
        (string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\"")
        |> mapError (Basics.always [ "string" ])

ident : Parser s Expression
ident =
    map EIdentifier (regex "[_a-zA-Z][_a-zA-Z0-9]*")
        |> mapError (always ["identifier"])

atom : Parser s Expression
atom =
  lazy <|
      \() -> choice [ bool, int, str, ident ]
          



