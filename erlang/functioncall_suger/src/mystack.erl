-module(mystack).

-export( [ create/0
         , push/2
         , pop/1
         ]).

-export_type([ stack/1
             , maybe/1
             ]).

-type stack(A) :: #{stack := [A] }.
-type maybe(A) :: {just, A} | nothing.


-spec create() -> stack(any()).
create() ->
    #{stack => []}.

-spec push(A, stack(A)) -> stack(A).
push(Value, This) ->
    #{stack => [Value | maps:get(stack, This)] }.

-spec pop(stack(A)) -> {maybe(A), stack(A)}.
pop(This) ->
    case maps:get(stack, This) of
        [H|T] ->
            {{just, H}, #{stack => T}};
        _ ->
            {nothing, This}
    end.
