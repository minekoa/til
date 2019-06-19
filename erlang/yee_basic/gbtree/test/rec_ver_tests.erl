-module(rec_ver_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test () ->
    D0 = gbtree2:init(),

    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError({key_exists, _},
                 gbtree2:insert({<<"Jhon">>, 58, <<"old-jhon@ymir.co.jp">>}, D3)
                ).

lookup_test () ->
    D0 = gbtree2:init(),

    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {value, {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}}
                , gbtree2:lookup(<<"Jhon">>, D3) ),

    ?assertEqual( none
                , gbtree2:lookup(<<"Alice">>, D3) ).


get_test () ->
    D0 = gbtree2:init(),

    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}
                , gbtree2:get(<<"Jhon">>, D3) ),

    ?assertError( not_exist
                , gbtree2:get(<<"Alice">>, D3) ).


exists_test () ->
    D0 = gbtree2:init(),
    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual(true,  gbtree2:exists(<<"Jhon">>, D3) ),
    ?assertEqual(false, gbtree2:exists(<<"Alice">>, D3) ).


update_test () ->
    D0 = gbtree2:init(),
    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = gbtree2:update(<<"Jhon">>, {40, <<"jhonjhon@ymir.co.jp">>}, D3),

    ?assertEqual( {<<"Jhon">>, 40, <<"jhonjhon@ymir.co.jp">>}
                , gbtree2:get(<<"Jhon">>, D4) ).

update_failure_test () ->
    D0 = gbtree2:init(),
    D1 = gbtree2:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree2:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree2:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError( not_exist
                , gbtree2:update(<<"Alice">>, {12, <<"alice2@ymir.co.jp">>}, D3)
                ).
