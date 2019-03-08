
まずは普通の。

```erlang
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
```

走らせてみる。

```console
## STYLE1
CREATE: #{stack => []}
PUSH 1: #{stack => [1]}
PUSH 3: #{stack => [3,1]}
PUSH 5: #{stack => [5,3,1]}
POP   : {just,5}, #{stack => [3,1]}
POP   : {just,3}, #{stack => [1]}
POP   : {just,1}, #{stack => []}
```

ふむん。

さて、次はこれだ。

```erlang
call_style2() ->
    io:format("## STYLE2~n"),

    StackA = mystack:create(),
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
```


ありゃん、エラーになった。

```
## STYLE2
CREATE: #{stack => []}
escript: exception error: bad argument
  in function  apply/3
     called as apply(#{stack => []},push,[1])
  in call from erl_eval:do_apply/6 (erl_eval.erl, line 680)
  in call from erl_eval:expr/5 (erl_eval.erl, line 449)
  in call from escript:eval_exprs/5 (escript.erl, line 872)
  in call from erl_eval:local_func/6 (erl_eval.erl, line 567)
  in call from escript:interpret/4 (escript.erl, line 788)
  in call from escript:start/1 (escript.erl, line 277)
  in call from init:start_em/1
Makefile:68: recipe for target 'run' failed
make: *** [run] Error 127
```

やっぱダメなのかな…。ドキュメント調べてもなにも乗ってなかったよね…。
って、まてまてまて。

bad argumentなのか？

apply/3の bad argument エラーだ

```
     called as apply(#{stack => []},push,[1])
```

これなんかありそうだな。

なんかやってるコード見たら、モジュール名のレコードつくって
それを型として扱ってるんだよな。

真似してみた。

```
escript: exception error: bad argument
  in function  apply/3
     called as apply({mystack2,[]},push,[1])
```

結果かわらん。そりゃそうだ。
コンパイルに干渉してるのかな？

わからん。
あきらめる。

仕組みよくわかんないよね。
