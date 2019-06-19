-module(tests).
-include_lib("eunit/include/eunit.hrl").

key_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo">>)
                , { query, <<"foo">>, none, none, desc }
                ).

key_duplicate_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&key=bar">>)
                , { query, <<"foo">>, none, none, desc }
                ).

key_not_found_test () ->
    ?assertError( {invalid_query, <<"key is not found">>}
                , querystring:list_condition_from_qs(<<"keyy=foo">>)
                ).

from_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&from=1999-12-21">>)
                , { query, <<"foo">>, {value, {1999,12,21}}, none, desc }
                ).

from_duplicate_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&from=1999-12-21&from=2001-01-10">>)
                , { query, <<"foo">>, {value, {1999,12,21}}, none, desc }
                ).

from_invalid_format_test () ->
    ?assertError( {invalid_query, [<<"invalid date format">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&from=1999-12-21-05">>)
                ),
    ?assertError( {invalid_query, [<<"invalid date format">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&from=1999-AA-21">>)
                ).

from_invalid_date_test () ->
    ?assertError( {invalid_query, [<<"invalid date">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&from=1999-02-30">>)
                ),
    ?assertError( {invalid_query, [<<"invalid date">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&from=1999-13-01">>)
                ).

to_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&to=1999-12-21">>)
                , { query, <<"foo">>, none, {value, {1999,12,21}}, desc }
                ).

to_duplicate_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&to=1999-12-21&to=2001-01-10">>)
                , { query, <<"foo">>, none, {value, {1999,12,21}}, desc }
                ).

to_invalid_format_test () ->
    ?assertError( {invalid_query, [<<"invalid date format">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&to=1999-12-21-05">>)
                ),
    ?assertError( {invalid_query, [<<"invalid date format">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&to=1999-AA-21">>)
                ).

to_invalid_date_test () ->
    ?assertError( {invalid_query, [<<"invalid date">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&to=1999-02-30">>)
                ),
    ?assertError( {invalid_query, [<<"invalid date">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&to=1999-13-01">>)
                ).

direction_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&direction=desc">>)
                , { query, <<"foo">>, none, none, desc }
                ),
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&direction=asc">>)
                , { query, <<"foo">>, none, none, asc }
                ).

direction_duplicate_test () ->
    ?assertEqual( querystring:list_condition_from_qs(<<"key=foo&direction=asc&direction=desc">>)
                , { query, <<"foo">>, none, none, asc }
                ).

direction_invalid_date_test () ->
    ?assertError( {invalid_query, [<<"invalid direction">>|_]}
                , querystring:list_condition_from_qs(<<"key=foo&direction=hoge">>)
                ).
