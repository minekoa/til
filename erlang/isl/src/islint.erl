-module(islint).
-export([from_string/1, from_file/1]).

from_string(String) ->
    {ok, Tokens, _} = islscanner:string(String),
    {ok, Ast}       = islparser:parse(Tokens),
    Ast.

from_file( FileName ) ->
    {ok, F } = file:read_file(FileName),
    from_string(binary_to_list(F)).

