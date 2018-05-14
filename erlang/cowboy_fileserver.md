# Cowboy で ファイルサーバーをつくるトライ

[cowboy/examples/file_server](https://github.com/ninenines/cowboy/tree/master/examples/file_server) を参考に作ってみるもハマりまくっていたのでアレゲ。
[Static handler](https://ninenines.eu/docs/en/cowboy/1.0/guide/static_handlers/) の使い方がいまいちうまく行かなかった。

うまく行く方法がとりあえず見つかったので、メモしてみる。

## Cowboy 2.0 用の Example を使っていなかった

いきなりやらかしたかな、という感じだけど、Cowboy 2.0.0 では Exampleが変わったようだ...orz

-[cowboy/examples/web_server](https://github.com/ninenines/cowboy/tree/2.0.0-pre.3/examples/web_server)

example 名が変わっている以外にも、`cowboy:start_clear` から `cowboy:start_htmp` に変わっていたり、
`directory_lister` がちょっとコードに修正があったり（でも動きに影響するほどじゃなさそうにみえる）、
小さな修正に見える。

とりあえずやってみよう。


### 雛形の作成

実際のアプリの作成のテストなので、rebar3 でビルドできるようにつくりたい。

まずは、テンプレート作成機能で作り始めよう

```sh
$ rebar3 new application file_server
```

```
.
├── LICENSE
├── README.md
├── rebar.config
├── rebar.lock
└── src
    ├── file_server.app.src
    ├── file_server_app.erl
    └── file_server_sup.erl
```


### rebar.config に依存ライブラリを追加

```erlang:rebar.config
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.3"}}},
  {jsx   , ".*", {git, "git://github.com/talentdeficit/jsx", {tag, "2.8.0"}}}
]}.
```

cowboy と JSX に依存するのでそれを追加。

### アプリケーションリストに cowboy を追加

`applications` のリスト、`stdlib` の後ろに `cowboy` を追加。

```erlang:src/file_server.app.src
  {application, file_server,
   [{description, "An OTP application"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, { file_server_app, []}},
    {applications,
     [kernel,
-     stdlib
+     stdlib,
+     cowboy
     ]},
    {env,[]},
    {modules, []},
   
    {maintainers, []},
    {licenses, []},
    {links, []}
   ]}.
```

### アプリケーションの作成

生成された雛形 file_server_app.erl に cowboy のルーティングと、起動のコードを書く。

```erlang:src/file_server_app.erl
  %%%-------------------------------------------------------------------
  %% @doc file_server public API
  %% @end
  %%%-------------------------------------------------------------------
   
  -module(file_server_app).
   
  -behaviour(application).
   
  %% Application callbacks
  -export([start/2, stop/1]).
   
  %%====================================================================
  %% API
  %%====================================================================
   
  start(_Type, _Args) ->
+	  Dispatch = cowboy_router:compile([
+		{'_', [
+			{"/[...]", cowboy_static, {priv_dir, file_server, "", [
+				{mimetypes, cow_mimetypes, all},
+				{dir_handler, directory_handler}
+			]}}
+		]}
+	  ]),
+	  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
+		{env, [{dispatch, Dispatch}]},
+		{middlewares, [cowboy_router, directory_lister, cowboy_handler]}
+	  ]),
      file_server_sup:start_link().
   
  %%--------------------------------------------------------------------
  stop(_State) ->
      ok.
   
  %%====================================================================
  %% Internal functions
  %%====================================================================
```

`dir_handler` として `directory_handler` を追加する。

あと、このサンプルの面妖なところなのだけれども、`cowboy_middlewares` として 新たに `directory_lister` を追加している。
（ここの理解が追いついていない）

ちなみに `priv_dir` とか `priv_file` とか は、Applicaton Root の直下の priv ファイルを示すみたい。ここに乗っといといたほうが楽できそうだけれども、
任意の dir を使いたいときは dir とか file とかのキーワードをつかえるように static_handler のリファレンスには書いてある。

[Nine Nines: cowboy_static(3)](https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy_static/)

では、handler から。

```erlang:src/directory_handler.erl
%% @doc Directory handler.
-module(directory_handler).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([list_json/2]).
-export([list_html/2]).

init(Req, Paths) ->
	{cowboy_rest, Req, Paths}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

resource_exists(Req, {ReqPath, FilePath}) ->
    io:format("resouce_exists?~n"),
	case file:list_dir(FilePath) of
		{ok, Fs} -> {true, Req, {ReqPath, lists:sort(Fs)}};
		_Err -> {false, Req, {ReqPath, FilePath}}
	end.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, list_json},
		{{<<"text">>, <<"html">>, []}, list_html}
	], Req, State}.

list_json(Req, {Path, Fs}) ->
	Files = [[ <<(list_to_binary(F))/binary>> || F <- Fs ]],
	{jsx:encode(Files), Req, Path}.

list_html(Req, {Path, Fs}) ->
	Body = [[ links(Path, F) || F <- [".."|Fs] ]],
	HTML = [<<"<!DOCTYPE html><html><head><title>Index</title></head>",
		"<body>">>, Body, <<"</body></html>\n">>],
	{HTML, Req, Path}.

links(<<>>, File) ->
	["<a href='/", File, "'>", File, "</a><br>\n"];
links(Prefix, File) ->
	["<a href='/", Prefix, $/, File, "'>", File, "</a><br>\n"].
```

resource_exists コールバックの結果が false だと reachable でないという判断になるみたい(404)。
Webサーバーをみていると無応答になるように見えるのだけれども、それでいいのか？

`content_types_provided/2` で、content_type に応じた処理の切り分けができる。
jsonできたらjson、htmlできたらhtmlで返す。


その他コールバックについては、[REST handlers](https://ninenines.eu/docs/en/cowboy/2.0/guide/rest_handlers/) を見ればいいのかな？

```erlang:src/directory_lister.erl
%% Feel free to use, reuse and abuse the code in this file.

-module(directory_lister).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    io:format("exec! ~p~n", [Env]),
	case lists:keyfind(handler, 1, Env) of
		{handler, cowboy_static} -> redirect_directory(Req, Env);
		_H -> {ok, Req, Env}
	end.

redirect_directory(Req, Env) ->
	Path = cowboy_req:path_info(Req),
    io:format("redirect! ~p~n", [Path]),
	Path1 = << <<S/binary, $/>> || S <- Path >>,
	{handler_opts, {_, _, _, Extra}} = lists:keyfind(handler_opts, 1, Env),
	{dir_handler, DirHandler} = lists:keyfind(dir_handler, 1, Extra),
	FullPath = resource_path(Path1),
	case valid_path(Path) and filelib:is_dir(FullPath) of
		true -> handle_directory(Req, Env, Path1, FullPath, DirHandler);
		false -> {ok, Req, Env}
	end.

handle_directory(Req, Env, Prefix, Path, DirHandler) ->
    io:format("handle_dir! ~p~n", [Path]),
	Env1 = lists:keydelete(handler, 1,
		lists:keydelete(handler_opts, 1, Env)),
	{ok, Req, [{handler, DirHandler}, {handler_opts, {Prefix, Path}} | Env1]}.

valid_path([]) -> true;
valid_path([<<"..">> | _T]) -> false;
valid_path([<<"/", _/binary>> | _T]) -> false;
valid_path([_H | Rest]) -> valid_path(Rest).

resource_path(Path) ->
	filename:join([code:priv_dir(file_server), Path]).
```

`cowboy-middleware` はハンドラーが噛むまえにexecute が実施されるのかな？

このmiddleware は `cowboy_static` ハンドラーが来る場合に限り、`redirect_directory` を実施します。


## ビルドの仕方（リリースビルドしないとだめ？）

Cowboy のサンプルは 独自の Makefile を定義していてそれを使ってビルドされていたけれど、rebar3 を使いたい……が、これがいけないのではないだろうか。

以下を参考にビルドしてみた。

-[erlang + rebar + cowboy Hello World](https://gist.github.com/flbuddymooreiv/ce1d7a47b12c27bf1616)

(先ほどdepsにcowboyを追加した)rebar.config にさらに `rebar_run plugin` を追加した。

```erlang:rebar.config
{erl_opts, [debug_info]}.
{deps, [
  {cowboy, ".*", {git, "git://github.com/ninenines/cowboy" , {tag, "2.0.0-pre.3"}}},
  {jsx   , ".*", {git, "git://github.com/talentdeficit/jsx", {tag, "2.8.0"}}}
]}.

{plugins, [rebar3_run]}.
```

あと、エラーメッセージをみるとリリースビルドできてなさそうだったので、 relx.config も追加。

```erlang:relx.config
{release, {file_server, "1"}, [file_server]}.
{extended_start_script, true}.
```

この状態で `rebar3 run` を実行する

```sh
$ rebar3 run
===> Verifying dependencies...
===> Compiling file_server
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /home/keijuterazono/projects/file_server/_build/default/lib
          /usr/lib/erlang/lib
          /home/keijuterazono/projects/file_server/_build/default/rel
===> Resolved file_server-1
===> Including Erts from /usr/lib/erlang
===> release successfully created!
===> No release to run was found.
```

残念ながら、`No release to run was found` エラーになってしまう。これなんだろう。
よくわからないが、未解決のママ進行させる。

`./_build/default/rel/file_server/bin/file_server console` で実行する.

```sh
$ ./_build/default/rel/file_server/bin/file_server console
Exec: /home/keijuterazono/projects/file_server/_build/default/rel/file_server/erts-8.1/bin/erlexec -boot /home/keijuterazono/projects/file_server/_build/default/rel/file_server/releases/1/file_server -mode embedded -boot_var ERTS_LIB_DIR /home/keijuterazono/projects/file_server/_build/default/rel/file_server/erts-8.1/../lib -config /home/keijuterazono/projects/file_server/_build/default/rel/file_server/releases/1/sys.config -args_file /home/keijuterazono/projects/file_server/_build/default/rel/file_server/releases/1/vm.args -- console
Root: /home/keijuterazono/projects/file_server/_build/default/rel/file_server
/home/keijuterazono/projects/file_server/_build/default/rel/file_server
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
(file_server@LR-25)1>
```

これで概ね期待通りに動く。

