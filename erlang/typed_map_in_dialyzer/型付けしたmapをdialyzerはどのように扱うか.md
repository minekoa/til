# 型付けしたmap()をdialyzerはどのように扱うか

## 動機

## マップの型付け

型定義の書き方は

[Erlang -- Types and Function Specifications](http://erlang.org/doc/reference_manual/typespec.html)

を見ると良い。

Mapまでの他の直和をごっそり省いて抜粋

```erlang
  Type :: Map

  Map :: map()                                 %% denotes a map of any size
       | #{}                                   %% denotes the empty map
       | #{AssociationList}

  AssociationList :: Association
                   | Association, AssociationList

  Association :: Type := Type                  %% denotes a mandatory association
               | Type => Type                  %% denotes an optional association
```

なので、目論んでいた

```erlang
-type extended_password() :: maps:merge( hogelib:password()
                                       , #{ created_at := ytime:zoned_time() }
                                       ).
  %% hogelib:password にフィールドを一個足したい
```

のような書き方はできない。

※（ただし、dialyzer やコンパイラはこれに警告を出さない。あいつら `type` definition なんていい加減にしかよんでねぇんじゃないか？という疑念が募る）

## 実験

以下の2つの型を用意する

```erlang
-type typed_map() :: #{ name := binary(),
                        id   := pos_integer()
                      }.

-type typed_map_ex() :: #{ name  := binary(),
                           id    := pos_integer(),
                           email := binary()
                         }.
```

Erlangの mapの タイプ付自体へんなものだが（これはもう mapではないのでは？)、そこは見ないことにする。

無印は supertype, `_ex` はフィールドを一個足したsubtype・・という関係だ。


これらにそれぞれについて操作関数を容易する

```
-spec add_typed_map_ex( typed_map_ex(), [typed_map_ex()]
                      ) -> [typed_map_ex()].
add_typed_map_ex(M, Ms) -> [M|Ms].
```

```
-spec add_typed_map( typed_map(), [typed_map()]
                   ) -> [typed_map()].
add_typed_map(M, Ms) -> [M|Ms].
```

さて、実験開始だ。これら関数と型の組み合わせ実験だ。

```erlang
test1() ->
    M = #{name => <<"bob">>, id => 44},
    add_typed_map(M, []).

test2() ->
    Mex = #{name => <<"bob">>, id => 44, email => <<"alice@example.com">>},
    add_typed_map(Mex, []).

test3() ->
    M = #{name => <<"bob">>, id => 44},
    add_typed_map_ex(M, []).

test4() ->
    Mex = #{name => <<"bob">>, id => 44, email => <<"alice@example.com">>},
    add_typed_map_ex(Mex, []).
```

これをダイアライザにかける

```console
$ rebar3 dialyzer
```

結果は以下のようになる

```
  46: Function test2/0 has no local return
  48: The call typed_map_in_dialyzer:add_typed_map(Mex::#{'email':=<<_:136>>, 'id':=44, 'name':=<<_:24>>}, []) breaks the contract (typed_map(), [typed_map()]) -> [typed_map()]
  50: Function test3/0 has no local return
  52: The call typed_map_in_dialyzer:add_typed_map_ex(M::#{'id':=44, 'name':=<<_:24>>}, []) breaks the contract (typed_map_ex(), [typed_map_ex()]) -> [typed_map_ex()]
  78: Function add_typed_map_porimorphism/0 will never be called
```

驚いたことに `super_type` を引数にとる関数に `sub_type` を突っんだ場合もエラーになる。
ポリモーフィズムなんてなかったんだよ!


## 実行時はどうなる

これは dialyzer がエラーを吐くだけなので、ポリモーフィズムなコードは実際には動いてしまう。

これ、型の意図通り実行時エラーにしたいな、と思うとどうすればいのかわからない。
when でガードを書くためには bif でなければならない。
引数のパターンマッチで弾くにしても、「書いていないキー」が存在してもエラーにはならない。

どうにもこうにも振る舞いがあわせられないのだ。

できれば、

```
-spec add_typed_map( typed_map(), [typed_map()]
                   ) -> [typed_map()].
add_typed_map(M as typed_map(), Ms) -> [M|Ms].
```

みたいなことが出から言えばなぁとおもうのですよ。ほんと、Erlangの型付けってなんなんじゃろ。
