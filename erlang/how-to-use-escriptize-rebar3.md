# rebar3 で escript 


rebar3 でつくることのできる雛形

```
$ rebar3 new
app (built-in): Complete OTP Application structure.
cmake (built-in): Standalone Makefile for building C/C++ in c_src
escript (built-in): Complete escriptized application structure
lib (built-in): Complete OTP Library application (no processes) structure
plugin (built-in): Rebar3 plugin project structure
release (built-in): OTP Release structure for executable programs
```


これの escript を使ってみる


```
$ rebar3 new escript hello_erlando
```


こんなんが作られている

* rebar.config

```erlang
{erl_opts, [no_debug_info]}.
{deps, []}.

{escript_incl_apps,
 [hello_erlando]}.
{escript_main_app, hello_erlando}.
{escript_name, hello_erlando}.
{escript_emu_args, "%%! +sbtu +A0\n"}.

%% Profiles
{profiles, [{test,
             [{erl_opts, [debug_info]}
            ]}]}.
```


* src/hello_erlando.app.src

```erlang
{application, hello_erlando,
 [{description, "An escript"},
  {vsn, "0.1.0"},
  {registered, []},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

* src/hello_erlando.erl

```erlang
-module(hello_erlando).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Arg) ->
    io:format("Args: ~p~n", [Arg]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
```

こんなんできてる。


生成されるREADME.md を読めばわかるが、

```
$ rebar3 escriptize
```

でビルド、

```
$ _build/default/bin/hello_erlando
```

で実行される。

（これがスクリプトの本体で、ビルドした beem をインポートする様な記述っぽいのだけども
スクリプト本体が エスケープされてて人間には読めないシロモノになっている）



## 外部ライブラリをつかう

* [How do I use external libraries in an Erlang escript?| Stack overflow](https://stackoverflow.com/questions/20862186/how-do-i-use-external-libraries-in-an-erlang-escript)
* [escriptize - configuration : escript_incl_apps is ignored #1350](https://github.com/erlang/rebar3/issues/1350)


Escript 自体は、

```
#!/usr/bin/env escript
%%! -pa ../pgsql/ebin -Wall
```

みたいな感じでライブラリをインポートすればいい

で、rebar3 の escriptize でこれを吐き出させるには`rebar.config` の `escript_incl_apps`セクションに追記する必要がある。


```
{erl_opts, [no_debug_info]}.
{deps, [{erlando, ".*", {git, "git://github.com/rabbitmq/erlando.git", {branch, "master"}}}]}.

{escript_incl_apps,
 [hello_erlando, erlando]}.
```


## Dialyzer が通るようにする


```
$ rebar3 escriptize
$ rebar3 dialyzer
    ・
    ・
    ・
===> Error in dialyzing apps: Analysis failed with error:
Could not scan the following file(s):
  Could not get abstract code for: /home/zoni/work/reps/til/erlang/hello_erlando/_build/default/lib/hello_erlando/ebin/hello_erlando.beam
  Recompile with +debug_info or analyze starting from source code
Last messages in the log cache:
  Reading files and computing callgraph... 
```

というエラーになる

この `Could not get abstract code for: hogehoge.beam` なエラー、
コンパイル時にデバッグインフォつけてないと出るらしい。なるほど。

で、rebar3 new escript な雛形でつくられる 
rebar.config だと、`{erl_opts, [no_debug_info]}.`　になってた。
これを `{erl_opts, [debug_info]}` にしたら解決。

