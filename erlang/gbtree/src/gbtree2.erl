-module(gbtree2).

%% API exports
-export([ init/0
        , insert/2
        , lookup/2
        , get/2
        , exists/2
        , update/3
        %% , delete/2
        %% , map/2
        %% , filter/2
        %% , count/1
        %% , fold/3
        ]).


-record(bio, { age :: non_neg_integer()
             , email :: binary()
             }).

-type recode() :: #bio{}.
-type addr() :: {Name::binary(), Age::non_neg_integer(), Email::binary()}.

-type tree() :: gb_trees:tree(binary(), recode()).


%% Q8-1
%%    空のデータ領域を作成する init/1 を作る

-spec init() -> tree().
init() ->
    gb_trees:empty().


%% Q8-2
%%    名前、年齢、メールアドレスのセットで登録できる insert/2 関数を作る
%%    名前はユニークでなければならない。もし同じ名前で登録しようとした場合、エラーにする

-spec insert( addr(), tree() ) -> tree().
insert({Name, Age, Email}, Tree) ->
    gb_trees:insert(Name, #bio{age   = Age
                              ,email = Email
                              }
                   , Tree).


%% Q8-3
%%    2つの方法 lookup/2, get/2 で、名前をキーに情報を取り出せるようにせよ
%%    lookup/2 は 在るときは {value {Name, Age, Email}}, 無いときは none を返せ

-spec lookup( binary(), tree() )
            -> {value, addr()} | none.
lookup(Name, Tree) ->
    case
        gb_trees:lookup(Name, Tree)
    of
        {value, Rec} ->
            {value, {Name, Rec#bio.age, Rec#bio.email}};
        none ->
            none
    end.

-spec get( binary(), tree() )
            -> addr().
get(Name, Tree) ->
    case lookup(Name, Tree) of
        {value, V} -> V;
        none       -> error( not_exist )
    end.


%% Q8-4
%%     exists/2 で指定した名前のメンバーがいるかどうかBool値で返せ

-spec exists( binary(), tree() ) -> boolean().
exists(Name, Tree) ->
    case lookup(Name, Tree) of
        {value, _} -> true;
        none       -> false
    end.


%% Q8-5
%%    年齢、メールアドレスの情報を update/3 で更新できる。

-spec update( binary()
            , {Age::non_neg_integer(), Email::binary()}
            , tree() ) -> tree().
update(Name, {Age, Email}, Tree) ->
    try
        gb_trees:update(Name, #bio{age=Age, email=Email}, Tree)
    of
        Tree2 -> Tree2
    catch
        error:function_clause -> %% 変な例外でるな
            error( not_exist )
    end.


%% %% Q8-6
%% %%    名前を指定して delete/2 でデータを削除できる。

%% -spec delete( binary(), tree() ) -> tree().
%% delete(Name, Tree) ->
%%     try
%%         gb_trees:delete(Name, Tree)
%%     of
%%         Tree2 -> Tree2
%%     catch
%%         error:function_clause ->
%%             error( not_exist )
%%     end.


%% %% Q8-7
%% %%     map/2 関数を追加。
%% %%
%% %%     note:
%% %%         これは問題が良くない.
%% %%         * 使用例は map ではなく foreach に見える
%% %%         * mapだとすると, 引数的に key もう変更可能にみえる

%% -spec map( fun( (recode()) -> recode() )
%%          , tree()
%%          ) -> tree().
%% map(F, Tree) ->
%% % Keyが変更されない落とし穴あり
%% %    gb_trees:map( fun (_, Rec) -> F(Rec) end, Tree ).

%% % 効率は悪いが正しく動作するバージョン
%%     ToPair = fun ({Name, Age, Email}) -> {Name, {Name, Age, Email}} end,
%%     Lst = lists:map( fun(V) -> ToPair(F(V)) end
%%                    , gb_trees:values(Tree) ),
%%     gb_trees:from_orddict(Lst).


%% %% Q8-8
%% %%    filter/2 関数を追加

%% -spec filter( fun( (recode()) -> boolean() )
%%             , tree()
%%             ) -> [recode()].
%% filter(F, Tree) ->
%%     lists:filter(F, gb_trees:values(Tree)).


%% %% Q8-9
%% %%    データの件数を返す、 count/1 関数を追加せよ。

%% -spec count( tree() ) -> non_neg_integer().
%% count(Tree) ->
%%     gb_trees:size(Tree).


%% %% Q8-10
%% %%    foldl/3 関数を追加。また、この関数を使い、平均年齢を出力せよ。

%% -spec fold( fun( (recode(), A) -> A )
%%           , A
%%           , tree()
%%           ) -> A.
%% fold(F, Acc, Tree) ->
%%     Itr = gb_trees:iterator(Tree),
%%     fold_(F, Acc, gb_trees:next(Itr)).


%% fold_(_, Acc0, none) ->
%%     Acc0;
%% fold_(F, Acc0, {_, Rec, Itr}) ->
%%     Acc1 = F(Rec, Acc0),
%%     fold_(F, Acc1, gb_trees:next(Itr)).
