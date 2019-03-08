-module(mystack2).

-export( [ create/0
         , push/2
         , pop/1
         ]).

-export_type([ stack/1
             , maybe/1
             ]).

%-record( mystack2
-record( ?MODULE
       , {stack = erlang:error(required_field, [stack])}
       ).


-type stack(A) ::
%        #mystack2{
        #?MODULE{
           stack :: [A]
          }.

-type maybe(A) :: {just, A} | nothing.


-spec create() -> stack(any()).
create() ->
    #?MODULE{stack = []}.
%    #mystack2{stack = []}.

-spec push(A, stack(A)) -> stack(A).
push(Value, This) ->
%%    This#mystack2{
    This#?MODULE{
%%                  stack = [Value | This#mystack2.stack]
                  stack = [Value | This#?MODULE.stack]
                }.

-spec pop(stack(A)) -> {maybe(A), stack(A)}.
pop(This) ->
    case This#mystack2.stack of
        [H|T] ->
            { {just, H},
              This#mystack2{ stack = T }
            };
        _ ->
            {nothing, This}
    end.
