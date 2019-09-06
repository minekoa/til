# Practival TLA+ 学習メモ chap1 (その4)

## Temporal Properties

> 「ワイヤに障害が発生した場合、アカウントは変更されません」
> 2人の単純なケースでは、やや弱いが扱いやすい要件を確認しましょう。
>
> 「アカウントの最終値の合計は開始値の合計と同じです」
> `NoOverdrafts` と違い、これは時間プロパティ(Temporal Property)です。
> 単純な不変条件ちぇっくは、仕様内の全ての状態で有効であることを確認します。
> 時間プロパティは、アルゴリズムの開始から終了までの全ての可能な「寿命(lifetime)」が、
> シーケンス内の異なる状態を相互に関連付ける何かに従うかどうかをチェックします。
>
> データベースが「常に一貫性がある」か「最終的に一貫性が在る」かをチェックすることの
> 違いのように考えてください。
> (ただし、それは確認できる多くのことの一つにすぎませんが）。
>
> 定義済みの時間プロパティを使用すると、新しい仕様は次のようになります。

ほほー。

```
define
    NoOverdrafts == \A p \in people: acc[p] >= 0
    EventuallyConsistant == <> [](acc["alice"] + acc["bob"] = 10)
end define;
```

読めぬ。 `<>` と `[]` がわからない

> `<>[]` は「最終的に常に(eventurally-always)」演算子であり、アルゴリズムがなにをしても最終的に真でなければならないことを意味します
> しばらくの間はfalseである可能性があり、trueとfalseを数回切り替えることがりますが、trueで終了します.

なんぞそれ.

`eventually` は「起きうること」でよく使うけど
それと常にの合体はキモイというか変な感じ。

### モデルチェックを実行

[Model Overview] ページ -> [Properties] の下にあるみたい
（既に先着で Terminationが追加されてる)

で実行。エラーになる。

> Temporal properties were violated.



| Name                 | Value                                      |    |
|----------------------|--------------------------------------------|----|
| <initial predicate>  | State (num = 1)                            |    |
| - acc                | [alice -> 5, bob -> 5 ]                    |    |
| - amount             | <<1,1>>                                    |    |
| - pc                 | <<"CheckAndWithDraw", "CheckAndWithDraw">> |    |
| - people             | {"alice", "bob"}                           |    |
| - receiver           | <<"bob", "bob">>                           |    |
| - sender             | <<"alice", "alice">>                       |    |
| <CheckAndWithdraw .. | State (num = 2)                            |    |
| - acc                | [alice -> 4, bob -> 5 ]                    | !! |
| - amount             | <<1,1>>                                    |    |
| - pc                 | <<"CheckAndWithDraw", "Deposit">>          | !! |
| - people             | {"alice", "bob"}                           |    |
| - receiver           | <<"bob", "bob">>                           |    |
| - sender             | <<"alice", "alice">>                       |    |
| <CheckAndWithdraw .. | State (num = 3)                            |    |
| - acc                | [alice -> 3, bob -> 5 ]                    | !! |
| - amount             | <<1,1>>                                    |    |
| - pc                 | <<"Deposit", "Deposit">>                   | !! |
| - people             | {"alice", "bob"}                           |    |
| - receiver           | <<"bob", "bob">>                           |    |
| - sender             | <<"alice", "alice">>                       |    |
| <Deposit ..          | State (num = 4)                            |    |
| - acc                | [alice -> 3, bob -> 6 ]                    | !! |
| - amount             | <<1,1>>                                    |    |
| - pc                 | <<"Done", "Deposit">>                      | !! |
| - people             | {"alice", "bob"}                           |    |
| - receiver           | <<"bob", "bob">>                           |    |
| - sender             | <<"alice", "alice">>                       |    |
| <Stuttering>         | State (num = 5)                            |    |

なるほど `<Stuttering> ` とか見知らぬアクションが入ってる
シット！ ということ？

これどうやって治す？について、良い方法がないって書いてある、おいおい。

* check , withdrow, deposit を1ステップにすればいい？それ wireに時間がかかるってコア条件に違反してるよ
* TLA+ に withdrow と deposit の間でプロセスが途切れないと伝える。まぁ仕様は合格するけど、実装のしようがないよね
* プロジェクトマネージャを説得して EventuallyConsistant 要求を緩めさせる（あきらめる）
* 全く別の実装を試す
* NoOverdrafts要件を緩和する。（実際の銀行がやるのはこれ）。当座借越(overdraft)が発生しないことを保証するのではなく、当座借越(overdraft)が発生する可能性を低くして、発生位した後機の手順を整えるように務める

ほほー。

> Specifying safe transfers is surprisingly hard!

へへー。


## Summary

* 不変条件、演算子、及び関数 の説明
    * 3章
* TLCの使用
    * 4章の一部
* 並行性
    * 5章
* シット！ と時間特性
    * 6章
* PlusCal の基本
    * 2章
