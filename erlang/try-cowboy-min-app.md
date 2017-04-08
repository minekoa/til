# Cowboy で最小のアプリを作ってみる

とりあえず、お試しで触ってみる。

## 1. rebar3 で雛形作成

```sh
$ rebar3 new app red_webapi
===> Writing red_webapi/src/red_webapi_app.erl
===> Writing red_webapi/src/red_webapi_sup.erl
===> Writing red_webapi/src/red_webapi.app.src
===> Writing red_webapi/rebar.config
===> Writing red_webapi/.gitignore
===> Writing red_webapi/LICENSE
===> Writing red_webapi/README.md
```

こんなのがが作られる

```
.
├── LICENSE
├── README.md
├── rebar.config
└── src
    ├── red_webapi.app.src
    ├── red_webapi_app.erl
    └── red_webapi_sup.erl
```

## 2. Cowboy を使えるようにする

https://github.com/ninenines/cowboy/releases をみると、最新は 2.0.0-pre.3 なので、これをつかう

rebar.config に依存関係を書く

```erlang:rebar.config
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy", {tag, "2.0.0-pre.3"}}}
]}.
```

red_webapi.app.src の application に cowboyを追加する


```erlang:src/red_webapi.app.src
{application, red_webapi,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { red_webapi_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

## 3. HTTPサーバーの実装

まず、red_webapi.erl を作成し、start/0 で各アプリケーションを順番にstartさせます。

```erlang:src/red_webapi.erl
-module(red_webapi).
-export([start/0]).

start () ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(red_webapi).
```

次に、作られた red_webapi_app.erl の start/2 の中で、cowboy:start_http を 実行して httpサーバー立ち上げ処理を実装します。

```erlang:src/red_webapi_app.erl
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
%%               {"/static", spa_handler, []},
%%               {"/v1/scenario/logs", rest_scenlog_handler, []}
               {"/v1/scenarios", rest_scenario_handler, []}
              ]}
                                     ]),
    {ok, _} = cowboy:start_http(
                http,
                100,
                [{port, 8010}],
                [{env, [{dispatch, Dispatch}]}]
               ),

    red_webapi_sup:start_link().
```

最後に URLにディスパッチしたハンドラーの本体を実装します。rest の場合、cowboy_rest モジュールのプラグインを書いてでよろしくやってもらいます。

```erlang:src/rest_scenario_handler.erl
-module(rest_scenario_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([scenarios_json/2]).

-define(SCENARIO_PATH,
        "/home/hoge/projects/RED/controller/src/application/main/_debug_host_SEED-MS3A_KISA0/.RED/preference/scenario/").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"text/json">>, scenarios_json}
     ], Req, State}.

scenarios_json(Req, State)->
    {ok, FNameList} = file:list_dir(?SCENARIO_PATH),
    Body = list_to_binary(
             "{\"scenarios\": [" 
             ++ string:join( lists:map(fun(FName) -> io_lib:format("\"~s\"",[FName]) end,
                                       FNameList),
                             ", ")
             ++ "]}"),
    {Body, Req, State}.
```

## 4. ビルドして実行

```sh
$ rebar3 upgrade
===> Verifying dependencies...
===> Fetching cowboy ({git,"git://github.com/ninenines/cowboy",
                                  {tag,"2.0.0-pre.3"}})
===> Fetching cowlib ({git,"https://github.com/ninenines/cowlib",
                                  "master"})
===> WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.
===> Fetching ranch ({git,"https://github.com/ninenines/ranch","1.1.0"})
===> WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.
===> No upgrade needed for cowboy
===> No upgrade needed for cowlib
===> Upgrading ranch ({git,"https://github.com/ninenines/ranch",
                                  "1.1.0"})
===> WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.
```

```sh
$ rebar3 compile
===> Verifying dependencies...
===> Compiling red_webapi
```


以下で実行する。

```sh
$ erl -pa ebin _build/default/lib/*/ebin -s red_webapi -noshell
```

## 参考

* [erlang + rebar3 + cowboyの最小構成 -Qiita](http://qiita.com/yu-sa/items/c33e9c155177a7e01f48)
* [Cowboy REST JSON API Guide](http://efene.org/guides/cowboy-rest-json.html)
* [ninenines/cowboy/excample/rest_hello_world -github](https://github.com/ninenines/cowboy/excample/rest_hello_world)


