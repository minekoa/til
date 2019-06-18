-module(tests).
-include_lib("eunit/include/eunit.hrl").

insert_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError({key_exists, _},
                 gbtree:insert({<<"Jhon">>, 58, <<"old-jhon@ymir.co.jp">>}, D3)
                ).

lookup_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {value, {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}}
                , gbtree:lookup(<<"Jhon">>, D3) ),

    ?assertEqual( none
                , gbtree:lookup(<<"Alice">>, D3) ).

get_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( {<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}
                , gbtree:get(<<"Jhon">>, D3) ),

    ?assertError( not_exist
                , gbtree:get(<<"Alice">>, D3) ).


exists_test () ->
    D0 = gbtree:init(),
    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual(true,  gbtree:exists(<<"Jhon">>, D3) ),
    ?assertEqual(false, gbtree:exists(<<"Alice">>, D3) ).


update_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = gbtree:update(<<"Jhon">>, {40, <<"jhonjhon@ymir.co.jp">>}, D3),

    ?assertEqual( {<<"Jhon">>, 40, <<"jhonjhon@ymir.co.jp">>}
                , gbtree:get(<<"Jhon">>, D4) ).

update_failure_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError( not_exist
                , gbtree:update(<<"Alice">>, {12, <<"alice2@ymir.co.jp">>}, D3)
                ).


delete_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = gbtree:delete(<<"Jhon">>, D3),
    ?assertEqual( none, gbtree:lookup(<<"Jhon">>, D4) ).


delete_failure_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertError( not_exist
                , gbtree:delete(<<"Jhonnn">>, D3) ).


map_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = gbtree:map( fun ({Name, Age, Email}) -> {Name, Age +1, Email} end
                   , D3 ),

    ?assertEqual( {<<"Jhon">>, 21, <<"jhon@ymir.co.jp">>}, gbtree:get(<<"Jhon">>, D4)),
    ?assertEqual( {<<"Bob">> , 24, <<"bob@ymir.co.jp">> }, gbtree:get(<<"Bob">> , D4)),
    ?assertEqual( {<<"Nick">>, 22, <<"nick@ymir.co.jp">>}, gbtree:get(<<"Nick">>, D4)).


map_keyupdate_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    D4 = gbtree:map( fun ({Name, Age, Email}) -> {<<"Mr.", Name/binary>>, Age, Email} end
                   , D3 ),

    ?assertEqual( {<<"Mr.Jhon">>, 20, <<"jhon@ymir.co.jp">>}, gbtree:get(<<"Mr.Jhon">>, D4)),
    ?assertEqual( {<<"Mr.Bob">> , 23, <<"bob@ymir.co.jp">> }, gbtree:get(<<"Mr.Bob">> , D4)),
    ?assertEqual( {<<"Mr.Nick">>, 21, <<"nick@ymir.co.jp">>}, gbtree:get(<<"Mr.Nick">>, D4)).


filter_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( [{<<"Bob">> , 23, <<"bob@ymir.co.jp">>}]
                , gbtree:filter( fun({_, Age, _}) -> Age >= 22 end, D3) ).


count_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">>,  23, <<"bob@ymir.co.jp">>},  D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),

    ?assertEqual( 3, gbtree:count(D3) ).


fold_test () ->
    D0 = gbtree:init(),

    D1 = gbtree:insert({<<"Jhon">>, 20, <<"jhon@ymir.co.jp">>}, D0),
    D2 = gbtree:insert({<<"Bob">> , 23, <<"bob@ymir.co.jp">>} , D1),
    D3 = gbtree:insert({<<"Nick">>, 21, <<"nick@ymir.co.jp">>}, D2),
    D4 = gbtree:insert({<<"Ari">> , 22, <<"ari@ymir.co.jp">>} , D3),

    Sum =
        gbtree:fold( fun({_, Age, _}, Acc) -> Acc + Age end
                   , 0
                   , D4
                   ),
    ?assertEqual(21.5, (Sum / gbtree:count(D4))).
