-module(typed_map_in_dialyzer).

%% API exports
-export([ add_typed_map_ex/2
        , add_typed_map/2
        , dummy/0
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
-spec add_typed_map_ex(
        typed_map_ex(),
        [typed_map_ex()]
       ) -> [typed_map_ex()].
add_typed_map_ex(M, Ms) ->
    [M|Ms].

-spec add_typed_map(
        typed_map(),
        [typed_map_ex()]
       ) -> [typed_map_ex()].
add_typed_map(M, Ms) ->
    add_typed_map_ex( M#{email => <<"">>}
                    , Ms ).

-spec dummy() -> ok.
dummy() ->
%    Ret = add_typed_map_invalid1(#{name => <<"alice">>, id=>42}, []),
%   Ret = add_typed_map_invalid2(#{name => <<"alice">>, id=>42}, []),
    add_typed_map_porimorphism(),
    ok.



%% -spec add_typed_map_invalid1( typed_map(), [typed_map_ex()] ) -> [typed_map_ex()].
%% add_typed_map_invalid1(M, Ms) ->
%%     add_typed_map_ex( M, Ms ).
%%
%% % src/typed_map_in_dialyzer.erl
%% %   43: Invalid type specification for function typed_map_in_dialyzer:add_typed_map_invalid1/2. The success typing is (#{'email':=binary(), 'id':=pos_integer(), 'name':=binary()},[#{'email':=binary(), 'id':=pos_integer(), 'name':=binary()}]) -> [#{'email':=binary(), 'id':=pos_integer(), 'name':=binary()},...]
%% %   47: Function add_typed_map_invalid1/2 will never be called

%% -spec add_typed_map_invalid2( typed_map(), [typed_map_ex()] ) -> [typed_map_ex()].
%% add_typed_map_invalid2(_, Ms) ->
%%     add_typed_map_ex( #{}, Ms ).
%%
%% % src/typed_map_in_dialyzer.erl
%% %   43: Function dummy/0 has no local return
%% %   61: Function add_typed_map_invalid2/2 has no local return
%% %   62: The call typed_map_in_dialyzer:add_typed_map_ex(#{}, Ms::[]) breaks the contract (typed_map_ex(), [typed_map_ex()]) -> [typed_map_ex()]


add_typed_map_porimorphism() ->
    Mex = #{name => <<"alice">>, id => 42, email => <<"alice@example.com">>},
    %% M   = #{name => <<"bob">>, id => 44},

    R1 = add_typed_map(Mex, []).
    %% R2 = add_typed_map_ex(Mex, R1),
    %% R3 = add_typed_map(M, R2),
    %% R4 = add_typed_map_ex(Mex, R3),
    %% R4.

%  74: The call typed_map_in_dialyzer:add_typed_map(Mex::#{'email':=<<_:136>>, 'id':=42, 'name':=<<_:40>>}, []) breaks the contract (typed_map(), [typed_map_ex()]) -> [typed_map_ex()]


%%====================================================================
%% Internal functions
%%====================================================================
