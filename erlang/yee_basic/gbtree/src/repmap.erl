-module(repmap).

-export([ init/0
        , insert/2
        , lookup/2
        , get/2
        , exists/2
        , update/3
        , delete/2
        , map/2
        ]).

-type name() :: binary().
-type bio()  :: { Name::name()
                , Age::non_neg_integer()
                , Email::binary()
                }.

-record( user, { age :: non_neg_integer()
               , email :: binary()
               } ).
-type user() :: #user{}.

-type user_map() :: #{name() := user()}.

-type maybe(A) :: {value, A} | none.


-spec init() -> user_map().
init() ->  #{}.

-spec insert(bio(), user_map()) -> user_map().
insert({Name, Age, Email}, Map) ->
    case maps:is_key(Name, Map) of
        false ->
            User = #user{age=Age, email=Email},
            Map#{Name => User };
        true ->
            error({key_exists, Name})
    end.


-spec lookup(name(), user_map()) -> maybe(bio()).
lookup(Name, Map) ->
    case
        maps:find(Name, Map)
    of
        {ok, Value} ->
            {value, {Name, Value#user.age, Value#user.email}};
        error ->
            none
    end.


-spec get(name(), user_map()) -> bio().
get(Name, Map) ->
    try
        #{Name := V} = Map,
        V
    of
        User -> {Name, User#user.age, User#user.email}
    catch
        error:{badmatch,_} ->
            error(not_exist)
    end.


-spec exists(name(), user_map()) -> boolean().
exists(Name, Map) ->
    maps:is_key(Name, Map).


-spec update( name()
            , {Age::non_neg_integer(), Email::binary()}
            , user_map()
            ) -> user_map().
update(Name, {Age, Email}, Map) ->
    try
        User = #user{age=Age, email=Email},
        Map#{Name := User}
    of
        M -> M
    catch
        error:{badkey, Name} ->
            error( not_exist )
    end.


-spec delete( name(), user_map() ) -> user_map().
delete(Name, Map) ->
    case maps:is_key(Name, Map) of
        true ->
            maps:remove(Name, Map);
        false ->
            error( not_exist )
    end.


-spec map( fun( (bio()) -> A )
         , user_map()
         ) -> #{name() := A }.
map(F, Map) ->
    maps:map( fun(K, V) ->
                      F({K, V#user.age, V#user.email})
              end
            , Map
            ).
