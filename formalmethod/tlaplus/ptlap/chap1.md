# Practival TLA+ 学習メモ chap1 (その1)

## 例題

```
EXTENDS Integers

(*--algorithm wire

variables
    people = {"alice", "bob"},
    acc = [p \in people |-> 5],
    sender = "alice",
    receiver = "bob",
    amount = 3;

define
    NoOverdrafts == \A p \in people: acc[p] >= 0
end define;

begin
    Withdraw:
        acc[sender] := acc[sender] - amount;
    Deposit:
        acc[receiver] := acc[receiver] + amount;
end algorithm;*)
```

変数と初期値はこんな感じかな

KMLライクに仕様を書くとこんな感じ

```
people   = {"alice", "bob"}
acc      = [{p: 5} | p <- people]
sender   = "alice"
receiver = "bob"
amount   = 3

state = withdrow -> deposit -> STOP

invariant {
    forall p <- people : acc[p] >= 0
}

[] withdrow -->
    post
       | state, acc[sender] |
       acc[sender]' = {sender: acc[sender] - amount}
       state' = deposit -> STOP

[] deposit ->
    post
        | state, acc[receiver] |
        acc[reciever]' = {reciever: acc[reciever] + amount}
        state' = STOP
```

状態遷移だけCSP的に書くと

```
αWire = {withdrow, deposit}
Wire = withdrow -> deposit -> STOP
```

かな。


アルゴリズムをなぜかコメント内に書いていくスタイル。

途中で種明かしされるけれど、これは PlusCal のコードで
Ctrl-T (あるいは [File] -> [Translate PlusCal Algorithm] で変換され
コメントしたに吐き出される。

PlusCal は TLA+ にいくつかの文法 (たとえば := 演算子など)を追加したもの、というスタンスのようだ

## PlusCal からトランスレートされたコードを読む


```tla+
\* BEGIN TRANSLATION
VARIABLES people, acc, sender, receiver, amount, pc

(* define statement *)
NoOverdrafts == \A p \in people: acc[p] >= 0


vars == << people, acc, sender, receiver, amount, pc >>

Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        /\ sender = "alice"
        /\ receiver = "bob"
        /\ amount = 3
        /\ pc = "Withdraw"

Withdraw == /\ pc = "Withdraw"
            /\ acc' = [acc EXCEPT ![sender] = acc[sender] - amount]
            /\ pc' = "Deposit"
            /\ UNCHANGED << people, sender, receiver, amount >>

Deposit == /\ pc = "Deposit"
           /\ acc' = [acc EXCEPT ![receiver] = acc[receiver] + amount]
           /\ pc' = "Done"
           /\ UNCHANGED << people, sender, receiver, amount >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == Withdraw \/ Deposit
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")
```

文法は基本的には、数式ベース。LaTeX で綺麗に見えればそれでいいという
割り切りがすごい。たとえば `\` はエスケープ文字で

* `\A`  .. ∀
* `\in` .. ∈

とか。

図形的なものとしては

* `/\` .. ∧
* `\/` .. ∨
* `[]` .. □
* `|->` .. `↦`

最後のは CSPの外部選択？

あと

* `==` は イコールの上に⊿があるあれのことらしい

### 変数

```
VARIABLES people, acc, sender, receiver, amount, pc

    ・
    ・
    ・

vars == << people, acc, sender, receiver, amount, pc >>
```

`pc` という変数が生えている.これは状態変数っぽい。
あと、 `vars` に入れている。よくわからない。。
`<< a,b,c,... >>` はもしかしてバリアント？

でも

```
UNCHANGED << people, sender, receiver, amount >>
```

とかでも使っているので、単なる変数のセットという意味かもしれない。

### 状態（なのかな？）

```
Withdraw == /\ pc = "Withdraw"
            /\ acc' = [acc EXCEPT ![sender] = acc[sender] - amount]
            /\ pc' = "Deposit"
            /\ UNCHANGED << people, sender, receiver, amount >>
```

をみると、まず `pc = "Withdraw"` とすることで、
遷移条件をガードしてるようだ。
（逆に言うと プライムつかない条件を `∧` で繋げば
ガードが作れるということか

`Init`, `Withdrow`, `Deposit`, がなにものかはよくわからない。
`==` の左辺値をなんて呼ぶんだろう。

ただ、これらの関係は

```
Spec        == Init ∧ □ [Next]_vars
Next        == Withdrow ∨ Deposit ∨ Terminating
```

ってなってる。 `Spec` は予約語てきなものなのかな？
じみに読めない。 たとえば `□` の意味がCSPの外部選択的ななにかなのか
しらないし、`[Next]_vars` の `[ ... ]_vars` もよくわからない.
コンテキスト的なもの？

`Terminating` は定義していない某だけど
自動生成されてるから予約語的なものじゃない（とおもう）

```
Terminating == pc = "Done" ∧ UNCHANGED vars
```

あ、ここで 全ての変数は変わらないとか期待から `vars` を定義したのかな？

だた、最後の

```
Termination == <>(pc = "Done")
```

は不明.
モデルチェックするときに停止するかを調べる条件式かな？と想像するけれど。

## モデルチェック

### 画面を開く

menu -> [TLC Model Checker] -> [New Model] でモデル作れる

[Model_1] タブができて
その中のインナータブに [Model Overview] [Model Checking Results] が在る感じ

[Model Overview] を見る。

[Whats is behavior spec?]
というところに、 Temporal fomula が選ばている時
テキストエリアに Spec という文字列がいる

これがさっきの仕様にでてきた Specかなあれかな。

### 不変条件をチェックする

で、チェックに向かう

[Invaliants]を開いて Addボタン
するとダイアログが開くのでそこに式をかく。
今回は NoOverdrafts。


でその状態でRunボタン(緑の矢印ボタン。ショートカットはF11)

あれー、The spec status is not "parsed". The status must be "parsed" before model checking is allowed. とかいわれた。
[file]->[Parse Model] をやってみる
・・・うまくいかない。
なんかいろいろやったら突然うまくいくようになったけれど、
画面下にSpecStatus というのがでてるので次はそこを見るように仕様

でもう一回。

[Model Checking Results] というのができてるのでみる

| Module | Action      | Location | Status Found | Distinct Status |
|--------|-------------|----------|--------------|-----------------|
| wire   | Terminating | ...      | 1            | 0               |
| wire   | Init        | ...      | 1            | 1               |
| wire   | Withdrow    | ...      | 1            | 1               |
| wire   | Deposit     | ...      | 1            | 1               |


なんだろこれ。

とりあえず Actionというなまえがついているのはわかった

本の法見ると

> You should see 4 states found, 3 distinct states, and no error.

らしい. distinct がよくわかんないな。明確な？明らかに違う？
Terminating がこれに該当しないのがよくわかんない。

そのうちわかるのかな？

一応最後の一節の翻訳

> TLCは、状態空間是体を徹底的に検索しました。トリッキーなことは何もし
> てなかったので、確認すべき振る舞いは一つだけでした
>
> 1. 可能な初期状態を選びます（可能な初期状態は一つだけです）
> 2. 開始状態が、不変条件 `NoOverdrafts` に従っているかを確認します。
>    そのため、評価を開始します。`Withdraw` step を実行します
> 3. 不変条件 `NoOverdrafts` を確認します、それは真なので、続けます
> 4. `Deposit` ステップを実行します
> 5. 不変条件 `NoOverdrafts` を確認します、それは真なので、完了です
>
>  仕様にエラーが在る場合は、エラーバーが表示されます。
>  それがおこらなかったので、私達の仕様はおそらく正しいのでしょう。
>  または、とてもシンプルすぎたのです。さぁ壊しましょう！
