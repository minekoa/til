# Makefile で引数をとるあれこれ

## 動機

`make test` でユニットテストが走るようにしたのだけれども
地味に件数が増えてくると全実行は辛いので、`make test hogehoge` 的な感じで指定できるようにしたいと思った

## 事実1. `make test hogehoge` は実は難しい

問題は、 `make` は、
コマンドラインのオプションでない引数をターゲットとして解釈すること。

引用: [make - Makefileの引数について | teratail](https://teratail.com/questions/35629)

> Makefileを実行時に引数を渡す方法として、 変数名を指定する方法があります。
>
> ```
> make run ARG=hoge
> ```
> しかし、このようにmakeを実行した場合、引数を受け取れるでしょうか？
>
> ```
> make run hoge
> ```
>
> 回答 1 件
> `make run hoge` と書くと、`run` も `hoge` もターゲットとみなされます。
> つまり、`make run ; make hoge` と同じです。


なので **本当にこれがしたいならば**
解決策は何もしない目標に変える、というトリックで解決することができる。

詳しくは以下の引用を参照。（でもこの方法にこだわる必要はないよね）


引用: [makefile 変数 パラメータ - "make run"に引数を渡す](https://code.i-harness.com/ja-jp/q/21caaf)

> ## (?) 環境変数 $1 分岐
>
> 私はMakefilesを使用します。
>
> ビルドターゲットを実行するrunというターゲットrunあります。
> 簡略化して、次のようになります。
>
> ```
> prog: ....
>   ...
>
> run: prog
>   ./prog
> ```
>
> 座って戻ってください。 私はこれが独創的であることを知っていますが、
> 起立騎乗の必要はありません。
>
> さて、私の質問は、引数を渡す方法はありますか？ そのため
>
> ```
> make run asdf --> ./prog asdf
> make run the dog kicked the cat --> ./prog the dog kicked the cat
> ```
>
> ありがとう！

> ## (i) 5 Answers
>
> GNU makeを使用している場合、これは簡単です。 唯一の問題は、 `make`が
> コマンドラインのオプションでない引数をターゲットとして解釈 `make` ことです。
> 解決策は何もしない目標に変えることです、 `make` は不平を言わないでしょう：
>
> ```
> # If the first argument is "run"...
> ifeq (run,$(firstword $(MAKECMDGOALS)))
>   # use the rest as arguments for "run"
>   RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
>   # ...and turn them into do-nothing targets
>   $(eval $(RUN_ARGS):;@:)
> endif
>
> prog: # ...
>     # ...
>
> .PHONY: run
> run : prog
>     @echo prog $(RUN_ARGS)
> ```
>
> これを実行すると、
>
> ```
> $ make run foo bar baz
> prog foo bar baz
> ```


## 作戦2. `make test TESTS=hogehoge` で行こう

普通に変数を渡すほうが素直です。これで万事OK

その上で、デフォルト値のある引数を作る方法ですが、
まずはトリッキーな方法。

引用: [Makefileでrunするときに引数を渡す | Qiita](https://qiita.com/tortuepin/items/9861c75853b516c8a279)

> ```
> ARG = foo
>
> a:  a.c
>     gcc -o a a.c
>
> run: a
>     ./a ${ARG}
> ```
>
> これでmake run ARG=hogeとかやると./a hogeを実行できる。
>
> MakefileのARG = fooよりも、実行時のARG=hogeの方が優先される。


「MakefileのARG = fooよりも、実行時のARG=hogeの方が優先される」。

そんな、叙述トリックみたいな、普通に読んでたらどうなってるのかわからないよね、というのが嫌ならば、

引用: [Makefileにパラメータを渡す方法と条件文 | Qiita](https://qiita.com/liubin/items/69d9faf804e82ddec376)

> ```
> # Makefile抜粋
>
> ifdef foo
>   FN=${foo}
> else
>   FN=default
> endif
> ```

だよねー。


## おまけ

引用: [GNU make変数の伝播についてまとめ | Qiita](https://qiita.com/aosho235/items/8cd094a2abd22bc62af0)

> ### 変数定義の優先順位
>
> 環境変数＜ファイル内に書いた値＜makeの引数

> ### 親makeから子makeへの変数の伝播の仕方
>
> * 親から子へ伝わるかどうか
> * 子で定義されている変数を上書きするかどうか
>
> を表にするとこうなる：
>
> |                 |-eなし                  |-eあり                  |
> |-----------------|------------------------|------------------------|
> |make HOGE=arg    |伝わる。上書きする      |伝わる。上書きする      |
> |HOGE=env make    |伝わる。上書きしない    |伝わる。上書きする      |
> |export HOGE=file1|伝わる。上書きしない    |伝わる。上書きする      |
> |HOGE=file1       |伝わらない。上書きしない|伝わらない。上書きしない|

> ## まとめ
>
> * 変数の優先順位は、環境変数＜ファイル内に書いた値＜makeの引数
> * exportした場合は環境変数と同じ扱いになる。したがって子に伝播する
> * exportしない場合はシェルにおけるシェル変数と同じく、自プロセス内でのみ有効（子に伝播しない）
> * 基本的に親ファイルに書いた変数は子ファイルに書いた変数を上書きしないが、`-e`をつけると上書きするようになる
> * コマンドラインの引数で指定した場合（`make HOGE=arg`）は常に子ファイルの変数を上書きする
