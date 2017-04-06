# Elm調査としてのデモ作成時のメモ

* サーバーサイド Erlang
* SPA Elm

の構成。Elmメモとしたが、Erlang側のお話も多し

## `XMLHttpRequest cannot load hogehoge` エラー

```
$ elm-reactor 
elm-reactor 0.18.0
Listening on http://localhost:8000
```

でやるとデバックしやすい。（ただし、WebAPI と SPA で別々に起動すると `XMLHttpRequest cannot load hogehoge` なエラーになるため、

```erlang
list_json(Req, Opt) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),

    {path, Path} = lists:keyfind(path, 1, Opt),
    FulPath = filename:join([code:priv_dir(webapi), Path]),
    case file:list_dir(FulPath) of
        {ok, Fs} -> 
            Files = lists:join( ",", [ io_lib:format("\"~s\"", [F]) || F <- Fs]),
            {list_to_binary("{\"scenarios\": [" ++ Files ++ "]}"), 
             Req2, Opt};

        _Err -> {false, Req, Path}
    end.
```

しするとよい。

* [XMLHttpRequest とは何か？ | 兵どもが、夢のあとさき](http://shogorobe.hatenablog.com/entry/2014/02/24/114956)
* [Enabling CORS for Cowboy REST API | Stack over flow](http://stackoverflow.com/questions/15616027/enabling-cors-for-cowboy-rest-api)


## JSONのエスケープ処理

* [elm-lang/core/4.0.1/Json.Encode](http://package.elm-lang.org/packages/elm-lang/core/4.0.1/Json-Encode)
* [JSON でのエスケープ処理 (JSONの値に""", "\" を含める場合の処理)](https://www.ipentec.com/document/document.aspx?page=json-character-escape)


## PUTができない?

Get ができていたので　Putもそのまま実装したら、何故か動かない

`415 unsupportedmediatype` が発生する。これは通常クライアント側の contents-type の指定漏れとかで発生するらしいのだが、
今回の場合はサーバー側、cowboy_rest の使い方が原因だった

* [Erlang Cowboy Rest Handler for POST request](http://stackoverflow.com/questions/17333308/erlang-cowboy-rest-handler-for-post-request)
* [Why got 415 unsupportedmediatype when using POST method](http://erlang.org/pipermail/erlang-questions/2015-April/084203.html)

`cowboy_rest:content_types_provided` は、メソッドとして GET,HEAD,POST,PUT,PATCH,DELETE を受け取ることができる。
でも、

(https://ninenines.eu/docs/en/cowboy/1.0/manual/cowboy_rest/)

> The ProvideResource value is the name of the callback that will be called if the content-type matches. It will only be called when a representation of the resource needs to be returned. It is defined as follow.
>  
> Methods: GET, HEAD
> Value type: iodata() | {stream, Fun} | {stream, Len, Fun} | {chunked, ChunkedFun}
> No default
> Return the response body.

なので、GETとHEAD以外の場合、callback に指定した関数が呼ばれない、というのが原因だった。


## Ajax による multipart/post でのファイルアップロード

[Ajaxによるmultipart/postでの画像ファイルアップロード その3](http://blog.asial.co.jp/1378)

