module HogeAtomExpressionTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Combine exposing (..)
import HogeLangParser


stripParserState ret =
  case ret of
    Ok (_, stream, result) ->
      Ok result

    Err (_, stream, errors) ->
      Err (String.join " or " errors)



suite : Test
suite =
    describe "HogeAtomExpression"
        [ test "int success (42)" <|
              \_ ->
                  parse HogeLangParser.int "42"
                      |> stripParserState
                      |> Expect.equal (Ok (HogeLangParser.EInt 42))

        , test "int success (-42)" <|
              \_ ->
                  parse HogeLangParser.int "-42"
                      |> stripParserState
                      |> Expect.equal (Ok (HogeLangParser.EInt -42))

        , test "int failre (abc)" <|
              \_ ->
                  parse HogeLangParser.int "abc"
                      |> stripParserState
                      |> Expect.equal (Err "integer")

        , test "bool success (true)" <|
              \_ ->
                  parse HogeLangParser.bool "true"
                      |> stripParserState
                      |> Expect.equal (Ok (HogeLangParser.EBool True))

        , test "bool success (false)" <|
              \_ ->
                  parse HogeLangParser.bool "false"
                      |> stripParserState
                      |> Expect.equal (Ok (HogeLangParser.EBool False))

        , test "bool failer (indeterminate)" <|
              \_ ->
                  parse HogeLangParser.bool "indeterminate"
                      |> stripParserState
                      |> Expect.equal (Err "boolean")

        , test "string success (\"abc\")" <|
              \_ ->
                  parse HogeLangParser.str "\"abc\""
                      |> stripParserState
                      |> Expect.equal (Ok (HogeLangParser.EString "abc"))

        ]

