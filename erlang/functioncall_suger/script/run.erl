#!/usr/local/ymir/erlang/bin/escript
%%! -pa ebin -Wall

main(_Arg) ->
    call_style1(),
    call_style1b(),
    call_style2().

call_style1() ->
    io:format("## STYLE1~n"),

    StackA = mystack:create(),
    io:format("CREATE: ~p~n",  [StackA]),

    StackA1 = mystack:push(1, StackA),
    io:format("PUSH 1: ~p~n",  [StackA1]),

    StackA2 = mystack:push(3, StackA1),
    io:format("PUSH 3: ~p~n",  [StackA2]),

    StackA3 = mystack:push(5, StackA2),
    io:format("PUSH 5: ~p~n",  [StackA3]),

    {V4, StackA4} = mystack:pop(StackA3),
    io:format("POP   : ~p, ~p~n",  [V4, StackA4]),

    {V5, StackA5} = mystack:pop(StackA4),
    io:format("POP   : ~p, ~p~n",  [V5, StackA5]),

    {V6, StackA6} = mystack:pop(StackA5),
    io:format("POP   : ~p, ~p~n",  [V6, StackA6]).


call_style1b() ->
    io:format("## STYLE1b~n"),

    StackA = mystack2:create(),
    io:format("CREATE: ~p~n",  [StackA]),

    StackA1 = mystack2:push(1, StackA),
    io:format("PUSH 1: ~p~n",  [StackA1]),

    StackA2 = mystack2:push(3, StackA1),
    io:format("PUSH 3: ~p~n",  [StackA2]),

    StackA3 = mystack2:push(5, StackA2),
    io:format("PUSH 5: ~p~n",  [StackA3]),

    {V4, StackA4} = mystack2:pop(StackA3),
    io:format("POP   : ~p, ~p~n",  [V4, StackA4]),

    {V5, StackA5} = mystack2:pop(StackA4),
    io:format("POP   : ~p, ~p~n",  [V5, StackA5]),

    {V6, StackA6} = mystack2:pop(StackA5),
    io:format("POP   : ~p, ~p~n",  [V6, StackA6]).

call_style2() ->
    io:format("## STYLE2~n"),

    StackA = mystack2:create(),
    io:format("CREATE: ~p~n",  [StackA]),

    StackA1 = StackA:push(1),
    io:format("PUSH 1: ~p~n",  [StackA1]),

    StackA2 = StackA1:push(3),
    io:format("PUSH 3: ~p~n",  [StackA2]),

    StackA3 = StackA2:push(3, StackA2),
    io:format("PUSH 5: ~p~n",  [StackA3]),

    {V4, StackA4} = StackA3:pop(),
    io:format("POP   : ~p, ~p~n",  [V4, StackA4]),

    {V5, StackA5} = StackA4:pop(),
    io:format("POP   : ~p, ~p~n",  [V5, StackA5]),

    {V6, StackA6} = StackA5:pop(),
    io:format("POP   : ~p, ~p~n",  [V6, StackA6]).
