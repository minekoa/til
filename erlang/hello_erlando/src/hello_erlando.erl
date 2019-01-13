-module(hello_erlando).

-import(maybe_m, ['>>='/2, return/1]).

-compile({parse_transform, do}).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main( any() ) -> no_return().
main(_Arg) ->
    Ret2 = routine2(),
    io:format("rutine2: (+2, 0) -> (0, +2) -> (+1, 0) ==> ~p~n", [Ret2]),
    Ret3 = routine3(),
    io:format("rutine3: (+1, 0) -> (0, +4) outi! -> (-1, 0) -> (0, -2) ==> ~p~n", [Ret3]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-type maybe(A) :: maybe_m:maybe(A).

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


-spec routine3() -> maybe(pole()).
routine3() ->
    do([maybe_m ||
           Start <- return({0, 0}),
           First <- land_left(1, Start),
           Second <- land_right(4, First),
           Third <- land_left(-1, Second),
           land_right(-2, Third)
       ]).



-spec land_left( 
        birds(),
        pole()
       ) -> maybe( integer() ).
land_left(N, {Left, Right}) ->
    case abs((Left + N) - Right) < 4 of
        true ->
            {just, {Left + N, Right}};
        false ->
            nothing
    end.


-spec land_right( 
        birds(),
        pole()
       ) -> maybe( pole() ).
land_right(N, {Left, Right}) ->
    case abs(Left - (Right + N)) < 4 of
        true ->
            {just, {Left, Right + N}};
        false ->
            nothing
    end.



