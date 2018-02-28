module Editor.Buffer exposing ( Model
                              , init

                              , Cursor
                              , nowCursorPos
                              , line

                              -- history
                              , EditCommand(Cmd_Insert, Cmd_Backspace, Cmd_Delete)

                              -- cursor move
                              , moveForward
                              , moveBackward
                              , movePrevios
                              , moveNext

                              -- edit
                              , insert
                              , backspace
                              , delete
                              , undo
                              )

type alias Model =
    { cursor : Cursor
    , contents : List String
    , history : List EditCommand
    }

init : String -> Model
init text =
    Model (Cursor 0 0)           -- cursor
          (String.lines text)    -- contents
          []                     -- history


-- buffer > cursor

type alias Cursor =
    { row : Int
    , column : Int
    }

defaultCursor : List String -> Cursor
defaultCursor contents =             
    let
        n = List.length contents
    in
        Cursor (if n < 0 then 0 else n) 0

nowCursorPos : Model -> (Int, Int)
nowCursorPos model = 
    ( model.cursor.row, model.cursor.column )
            

-- buffer > contents

line : Int -> List String -> Maybe String
line n lines =
    if n < 0
    then Nothing
    else List.head (List.drop n lines)

maxColumn: String -> Int
maxColumn line =
    (String.length line) - 1

maxRow : List String -> Int
maxRow contents =
    (List.length contents) - 1


------------------------------------------------------------
-- History
------------------------------------------------------------

type EditCommand
    = Cmd_Insert (Int, Int) String
    | Cmd_Backspace (Int, Int) String -- str は undo用
    | Cmd_Delete (Int, Int) String    -- str は undo 用
--    | Cmd_Undo EditCommand

appendHistory: EditCommand -> Model -> Model
appendHistory cmd model =
    case (cmd, List.head model.history) of
        ( (Cmd_Insert (r, c) s), Just (Cmd_Insert (or, oc) os) ) ->
            if (r == or) && (c == oc + (String.length os))
            then { model | history = (Cmd_Insert (or, oc) (os ++ s)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        ( (Cmd_Backspace (r, c) s), Just (Cmd_Backspace (or, oc) os) ) ->
            if (r == or) && (c == oc - (String.length os))
            then { model | history = (Cmd_Backspace (or, oc) (s ++ os)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        ( (Cmd_Delete (r, c) s), Just (Cmd_Delete (or, oc) os) ) ->
            if (r == or) && (c == oc)
            then { model | history = (Cmd_Delete (or, oc) (os ++ s)) :: List.drop 1 model.history }
            else { model | history = cmd :: model.history }

        (_ , _) ->
            { model | history = cmd :: model.history }

------------------------------------------------------------
-- Cursor move
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
        |> Maybe.withDefault (defaultCursor model.contents)
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
        |> Maybe.withDefault (defaultCursor model.contents)
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

------------------------------------------------------------
-- edit
------------------------------------------------------------

insert: (Int, Int) -> String -> Model -> Model
insert (row, col) text model =
    model
    |> insert_proc (row, col) text
    |> appendHistory (Cmd_Insert (row, col) text)

backspace: (Int, Int) -> Model -> Model
backspace (row, col) model =
    let
        (m, deleted) = backspace_proc (row, col) model 
    in
        case deleted of
            Nothing ->
                m
            Just s ->
                m
                |> appendHistory (Cmd_Backspace (row, col) s)

delete: (Int, Int) -> Model -> Model
delete (row, col) model =
    let
        (m, deleted) = delete_proc (row, col) model
    in
        case deleted of
            Nothing ->
                m
            Just s ->
                m
                |> appendHistory (Cmd_Delete (row, col) s)


undo : Model -> Model
undo model =
    case List.head model.history of
        Nothing -> model
        Just cmd ->
            ( case cmd of
                  Cmd_Insert (row, col) str    ->
                      undo_insert_proc (row, col) str model

                  Cmd_Backspace (row, col) str ->
                      undo_backspace_proc (row, col) str model

                  Cmd_Delete (row, col) str    ->
                      insert_proc (row, col) str model
            )
            |> (\ m -> {m | history = List.drop 1 m.history })


------------------------------------------------------------
-- (private) edit
------------------------------------------------------------

insert_proc: (Int, Int) -> String -> Model -> Model
insert_proc (row, col) text model =
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

backspace_proc: (Int, Int) -> Model -> (Model, Maybe String)
backspace_proc (row, col) model =
    case (row, col) of
        (0, 0) ->
            (model, Nothing)

        (_, 0) ->
            let
                prows  = List.take (row - 1) model.contents
                crow   = List.drop (row - 1) model.contents |> List.take 2 |> String.concat
                nrows  = List.drop (row + 1) model.contents

                n_col  = List.drop (row - 1) model.contents |> List.head |> Maybe.withDefault "" |> String.length
            in
                ( { model
                      | contents = prows ++ (crow :: nrows )
                      , cursor = Cursor (row - 1) (n_col)
                  }
                , Just "\n")

        (_, n) ->
            let
                prows = List.take row model.contents
                crow  = line row model.contents |> Maybe.withDefault ""
                nrows = List.drop (row + 1) model.contents

                left  = (String.left (col - 1) crow)
                right = (String.dropLeft (col) crow)
            in
                ( { model
                      | contents = prows ++ ((left ++ right) :: nrows)
                      , cursor = Cursor row  (col - 1)
                  }
                , Just (crow |> String.dropLeft (col - 1) |> String.left 1) )


delete_proc: (Int, Int) -> Model -> (Model, Maybe String)
delete_proc (row, col) model =
    let
        ln      = line row model.contents |> Maybe.withDefault ""
        max_row = maxRow model.contents
        max_col = maxColumn ln
    in
        case (row == max_row, col > max_col) of
             (True, True)  ->
                 (model , Nothing)

             (_   , False) ->
                 let
                     prows  = List.take row model.contents
                     nrows  = List.drop (row + 1) model.contents

                     current = (String.left (col) ln) ++ (String.dropLeft (col + 1) ln)
                 in
                     ( { model
                           | contents = prows ++ (current :: nrows)
                       }
                     , Just (ln |> String.dropLeft col |> String.left 1) )

             (_   , True) ->
                 let
                     prows  = List.take row model.contents
                     nxt    = line (row + 1) model.contents |> Maybe.withDefault ""
                     nrows  = List.drop (row + 2) model.contents

                     current = ln ++ nxt
                 in
                     ( { model
                           | contents = prows ++ (current :: nrows)
                       }
                     , Just "\n" )

    
undo_insert_proc : (Int, Int) -> String -> Model -> Model
undo_insert_proc (row, col) str model =
    -- todo: ちゃんと実装する。
    --       現存の編集イベントを組み合わせて強引に実現している。汚い。
    let
        delete_n = (\ c m ->
                        if c <= 0 then m
                        else backspace_proc (m.cursor.row, m.cursor.column) m
                             |> Tuple.first
                             |> delete_n (c - 1)
                   )
        ls      = String.lines str
        r_delta = (List.length ls) - 1
        c_delta = ls
                |> List.reverse 
                |> List.head
                |> Maybe.withDefault ""
                |> String.length
                |> (+) -1
    in
        delete_n (String.length str)
            { model
                | cursor = (Cursor (row + r_delta) (col + c_delta + 1))
            }

undo_backspace_proc : (Int, Int) -> String -> Model ->Model
undo_backspace_proc (row, col) str model =
    let
        ls      = String.lines str
        r_delta = (List.length ls) - 1
        c_delta = ls
                |> List.reverse 
                |> List.head
                |> Maybe.withDefault ""
                |> String.length
                |> (+) -1
    in
        insert_proc (row - r_delta, col - c_delta - 1) str model 


