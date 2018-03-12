module BufferSelectionTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


import Editor.Buffer as Buffer

suite : Test
suite =
    describe "Selection"
        [ test "default cursor pos" <|
              \_ ->
                  Buffer.init "ABC\nDE\nGHIJ\nK\n"
                      |> (.selection)
                      |> Expect.equal Nothing
        , test "selection start (forward)" <|
              \_ ->
                  Buffer.init "ABC\nDE\nGHIJ\nK\n"
                      |> Buffer.selectForward
                      |> Expect.all [ \m -> Expect.equal (Buffer.Range (0,0) (0,1) |> Just) m.selection
                                    , \m -> Expect.equal (0,1) (Buffer.nowCursorPos m)
                                    ]
        , test "selection start (backward)" <|
              \_ ->
                  Buffer.init "ABC\nDE\nGHIJ\nK\n"
                      |> Buffer.moveForward
                      |> Buffer.selectBackward
                      |> Expect.all [ \m -> Expect.equal (Buffer.Range (0,1) (0,0) |> Just) m.selection
                                    , \m -> Expect.equal (0,0) (Buffer.nowCursorPos m)
                                    ]
        , test "selection start (next)" <|
              \_ ->
                  Buffer.init "ABC\nDE\nGHIJ\nK\n"
                      |> Buffer.selectNext
                      |> Expect.all [ \m -> Expect.equal (Buffer.Range (0,0) (1,0) |> Just) m.selection
                                    , \m -> Expect.equal (1,0) (Buffer.nowCursorPos m)
                                    ]
        , test "selection start (previos)" <|
              \_ ->
                  Buffer.init "ABC\nDE\nGHIJ\nK\n"
                      |> Buffer.moveNext
                      |> Buffer.selectPrevios
                      |> Expect.all [ \m -> Expect.equal (Buffer.Range (1,0) (0,0) |> Just) m.selection
                                    , \m -> Expect.equal (0,0) (Buffer.nowCursorPos m)
                                    ]
        ]




