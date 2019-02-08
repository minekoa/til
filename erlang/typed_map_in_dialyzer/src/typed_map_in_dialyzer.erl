-module(typed_map_in_dialyzer).

%% API exports
-export([ add_typed_map_ex/2
        , add_typed_map/2
        , test1/0
        , test2/0
        , test3/0
        , test4/0
        ]).

-export_type([ typed_map/0
             , typed_map_ex/0
             ]).


-type typed_map() :: #{ name := binary(),
                        id   := pos_integer()
                      }.

-type typed_map_ex() :: #{ name  := binary(),
                           id    := pos_integer(),
                           email := binary()
                         }.



%%====================================================================
%% API functions
%%====================================================================
-spec add_typed_map_ex( typed_map_ex(), [typed_map_ex()]
                      ) -> [typed_map_ex()].
add_typed_map_ex(M, Ms) -> [M|Ms].

-spec add_typed_map( typed_map(), [typed_map()]
                   ) -> [typed_map()].
add_typed_map(M :: typed_map(), Ms) -> [M|Ms].


test1() ->
    M = #{name => <<"bob">>, id => 44},
    add_typed_map(M, []).

test2() ->
    Mex = #{name => <<"bob">>, id => 44, email => <<"alice@example.com">>},
    add_typed_map(Mex, []).

test3() ->
    M = #{name => <<"bob">>, id => 44},
    add_typed_map_ex(M, []).

test4() ->
    Mex = #{name => <<"bob">>, id => 44, email => <<"alice@example.com">>},
    add_typed_map_ex(Mex, []).


%%====================================================================
%% Internal functions
%%====================================================================
