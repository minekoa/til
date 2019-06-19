-module(querystring).

-export([list_condition_from_qs/1]).

-include("querystring.hrl").
-type query() :: #query{}.

-type prop_list() :: [{unicode:chardata(), unicode:chardata()}].


-spec list_condition_from_qs(uri_string:uri_string()) -> query().
list_condition_from_qs(QueryString) ->
    case
        uri_string:dissect_query(QueryString)
    of
        {error, A, B} ->
            error({invalid_query, [<<"parse failure">>, A, B]});
        PropList ->
            #query{ key       = get_key(PropList)
                  , from      = lookup_date(<<"from">>, PropList)
                  , to        = lookup_date(<<"to">>  , PropList)
                  , direction = get_direction_with_default(PropList)
                  }
    end.


-spec get_key(prop_list()) -> unicode:chardata().
get_key(PropList) ->
    case
        proplists:lookup_all(<<"key">>, PropList)
    of
        []      -> error({invalid_query, <<"key is not found">>});
        [{_, Value} | _] -> Value
    end.


-spec lookup_date( unicode:chardata()
                 , prop_list()
                 ) -> {value, calendar:date()} | none.
lookup_date(Tag, PropList) ->
    case
        proplists:lookup_all(<<"to">>, PropList)
    of
        []               -> none;
        [{_, Value} | _] -> {value, parse_date(Value)}
    end.


-spec parse_date(unicode:chardata()) -> calendar:date().
parse_date(DateString) ->
    try
        lists:map( fun (S) ->
                           {I, <<"">>} = string:to_integer(S),
                           I
                   end
                 , string:split(DateString,"-", all)
                 )
    of
        [Y, M, D] ->
            case calendar:valid_date(Y, M, D) of
                true  -> {Y,M,D};
                false -> error({invalid_query, [<<"invalid date">>, {Y,M,D}]})
            end;
        X ->
            error({invalid_query, [<<"invalid date format">>, DateString, X]})
    catch
        error:{badmatch,_} -> %% ちょっと乱暴
            error({invalid_query, [<<"invalid date format">>, DateString]})
    end.


-spec get_direction_with_default(prop_list()) -> desc | asc.
get_direction_with_default(PropList) ->
    case
        proplists:lookup_all(<<"direction">>, PropList)
    of
        []                     -> desc;
        [{_, <<"desc">>} | _]  -> desc;
        [{_, <<"asc">>}  | _]  -> asc;
        [{_, V} | _]  ->
            error({invalid_query, [<<"invalid direction">>, V]})
    end.
