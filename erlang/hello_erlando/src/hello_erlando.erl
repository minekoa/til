-module(hello_erlando).

-import(maybe_m, ['>>='/2, return/1]).


-compile({parse_transform, do}).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Arg) ->
    Ret = routine2(),
    io:format("Args: ~p~n", [Ret]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================


-type maybe(A) :: {just, A} | nothing.


-type birds() :: integer().
-type pole() :: {birds(), birds()}.


-spec routine2() -> maybe(pole()).
routine2() ->
    do([maybe_m ||
           Start <- return({0, 0}),
           First <- land_left(2, Start),
           Second <- land_right(2, First),
           land_left(1, Second)
       ]).


-spec land_left( 
        birds(), pole()
       ) -> maybe( integer() ).
land_left(N, {Left, Right}) ->
    case abs((Left + N) - Right) < 4 of
        true ->
            {just, {Left + N, Right}};
        false ->
            nothing
    end.

-spec land_right( 
        birds(), pole()
       ) -> maybe( pole() ).
land_right(N, {Left, Right}) ->
    case abs(Left - (Right + N)) < 4 of
        true ->
            {just, {Left, Right + N}};
        false ->
            nothing
    end.



