-module(map_var_test).
-include_lib("eunit/include/eunit.hrl").

insert_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError({key_exists, _},
                 repmap:insert({<<"Jhon">>, 58, <<"old-jhon@ymir.co.jp">>}, D3)
                ).

lookup_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {value, {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}}
                , repmap:lookup(<<"Jhon">>, D3) ),

    ?assertEqual( none
                , repmap:lookup(<<"Alice">>, D3) ).

get_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}
                , repmap:get(<<"Jhon">>, D3) ),

    ?assertError( not_exist
                , repmap:get(<<"Alice">>, D3) ).


exists_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual(true,  repmap:exists(<<"Jhon">>, D3) ),
    ?assertEqual(false, repmap:exists(<<"Alice">>, D3) ).


update_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = repmap:update(<<"Jhon">>, {40, <<"jhonjhon@ymir.co.jp">>}, D3),

    ?assertEqual( {<<"Jhon">>, 40, <<"jhonjhon@ymir.co.jp">>}
                , repmap:get(<<"Jhon">>, D4) ).

update_failure_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError( not_exist
                , repmap:update(<<"Alice">>, {12, <<"alice2@ymir.co.jp">>}, D3)
                ).


delete_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = repmap:delete(<<"Jhon">>, D3),
    ?assertEqual( none, repmap:lookup(<<"Jhon">>, D4) ).


delete_failure_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError( not_exist
                , repmap:delete(<<"Jhonnn">>, D3) ).

map_test () ->
    D0 = repmap:init(),
    D1 = repmap:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = repmap:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = repmap:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = repmap:map( fun ({Name, Age, Email}) -> {user, Age +1, Email} end
                   , D3 ),

    ?assertEqual( {<<"Jhon">>, 21, <<"jhon@ymir.co.jp">>}, repmap:get(<<"Jhon">>, D4)),
    ?assertEqual( {<<"Bob">> , 24, <<"bob@ymir.co.jp">> }, repmap:get(<<"Bob">> , D4)),
    ?assertEqual( {<<"Nick">>, 22, <<"nick@ymir.co.jp">>}, repmap:get(<<"Nick">>, D4)).
