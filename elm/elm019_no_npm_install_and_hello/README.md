# Elm 0.19 で npm を使わずにインストールして、小さなプロジェクトを作ってみる

## install

elm と elm-format をダウンロードしてパスの通っているところに展開する。
（ホームの下に展開してみた. 018との共存できるように頑張れるかな？）

### elm (compiler)

[download page](https://github.com/elm/compiler/releases/tag/0.19.0)

```
$ wget "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz"
$ tar xzf binaries-for-linux.tar.gz
$ mv elm ~/usr/local/bin/

```

0.19 から 全部ひっくるめて elm コマンドが担うことになった

* elm repl
* elm init
* elm reactor
* elm make
* elm install
* elm bump
* elm diff
* elm publish

ちなみに node いれてないと `elm repl` は使えない

### elm-format

[download page](https://github.com/avh4/elm-format/releases/tag/0.8.1)

```
$ wget "https://github.com/avh4/elm-format/releases/download/0.8.1/elm-format-0.8.1-linux-x64.tgz"
$ tar xzf elm-format-0.8.1-linux-x64.tgz
$ mv elm-format ~/usr/local/bin/
```


## Initialize my project.

[Elm 0.19 のおもな変更点](http://jinjor-labo.hatenablog.com/entry/2018/08/23/142026):

> `elm init` で最低限のプロジェクトを生成できるようになった

とあるので、やってみる

```
$ mkdir myui
$ cd myui
$ elm init
Hello! Elm projects always start with an elm.json file. I can create them!

Now you may be wondering, what will be in this file? How do I add Elm files to
my project? How do I see it in the browser? How will my code grow? Do I need
more directories? What about tests? Etc.

Check out <https://elm-lang.org/0.19.0/init> for all the answers!

Knowing all that, would you like me to create an elm.json file now? [Y/n]: y
Okay, I created it. Now read that link!
```

```
$ tree .
.
├── elm.json
└── src
```

本気でなんもないな。`elm.json` をみる

```
$ cat elm.json
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.1",
            "elm/core": "1.0.2",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```

これは

[Elm 0.19 のおもな変更点](http://jinjor-labo.hatenablog.com/entry/2018/08/23/142026):

> ## elm-package.json が elm.json になった
>
> アプリケーション用とライブラリ用で書き方が変わる。 アプリケーション
> の場合はバージョンが固定されて lock ファイルになる。
> test-dependencies という項目ができたのでテスト用にもう一つ JSON を用
> 意する必要がなくなった。 初期状態でインストールされるのは elm/core
> と elm/json の２つで、 HTML などは手動でインストールする必要がある。

とのこと。

## Starting: どういうアプリケーションにする？

0.18 に比べてわからない人が選びにくい名前になったな、と感じる。
「悪い雛形群」のアンチパターンに向かってる気がします。
0.20 があったらシンプル方向に揺り戻しあるかな？


[Browser - browser 1.0.1](https://package.elm-lang.org/packages/elm/browser/latest/Browser)

### Browser.sandbox

```elm
sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    }
    -> Program () model msg
```

elm の外に出ない構成。ぴゅあぴゅあ。

sandboxという名前は確かに体を表しているけれど、
本番で使うのを躊躇させる名前でもある。

### Browser.element

```elm
element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
```

portを介してelmの外とやり取りできる 0.18er おなじみの形式。
0.18にない要素として flags というのが initに増えているが、起動引数だと思えばいい（多分）

[フラグ An Introduction to Elm](https://guide.elm-lang.jp/interop/flags.html)

> フラグとして渡すことのできる型には、次のように様々な型があります。
>
> * Bool
> * Int
> * Float
> * String
> * Maybe
> * List
> * Array
> * tuples
> * records
> * Json.Decode.Value

好きなの渡せってことさ。

### Browser.document

```elm
document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }
```

0.18ユーザ待望のタイトル変更できる版。
タイトルくらい Ports つかわないでも変更させてやんよ！
という温かい心遣いを感じる

### Browser.application

```elm
application :
    { init : flags -> Url -> Key -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> Program flags model msg
```

SPA最強形態。URLがちゃんと変わるよ！とか、URLで飛べるよ！とか。
navigateができるようになる


とりあえず 0.18er は Browser.elementで作ってみよう。

## 簡単な画面を作ってみる

src/Main.elm

```elm
module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


type alias Model =
    { auth_id : String
    , auth_passwd : String
    }


type Msg
    = AuthId String
    | AuthPasswd String
    | Authentication


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthId id ->
            ( { model | auth_id = id }, Cmd.none )

        AuthPasswd passwd ->
            ( { model | auth_passwd = passwd }, Cmd.none )

        Authentication ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , value model.auth_id
            , onInput AuthId
            ]
            []
        , input
            [ type_ "password"
            , value model.auth_passwd
            , onInput AuthPasswd
            ]
            []
        , button [ onClick Authentication ] [ text "Authentication" ]
        ]
```

これを

```console
$ elm-format src/Main.elm --yes
$ elm make src/Main.elm --output=main.js
```

する。

`elm make` すれば、依存ライブラリの解決からするっと全部やってくれるので
`elm install` する必要はない。


main.jsができたら、HTMLファイルから

```html
<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>Main</title>
  <link rel="stylesheet" href="whatever-you-want.css">
  <script src="main.js"></script>
</head>
<body>
  <div id="elm"></div>
  <script>
    var app = Elm.Main.init({
        node: document.getElementById('elm'),
        flags:0
    });
  </script>
</body>
</html>
```

なふうに呼び出す感じでおけ。
