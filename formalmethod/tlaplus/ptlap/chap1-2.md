# Practival TLA+ 学習メモ chap1 (その2)

## Innitial Conditions

仕様壊すところから。

```
amount \ in 1..6
```


さいしょうっかり `amount = \in 1..6` とか書いたらエラーになった。
あれだとすると `amount = a \in 1..6` でいける？
→ aとかそんなオペレータしらんいわれた　


気を取り直して、モデルチェーーく

エラータブ開いたよ

とりま Checking Result から.

| Module | Action    | Location | Status | Distinct Status |    |
|--------|-----------|----------|--------|-----------------|----|
| wire   | Deposit   | ...      | 0      | 0               | !! |
| wire   | Treminate | ...      | 0      | 0               | !! |
| wire   | Init      | ...      | 6      | 6               |    |
| wire   | Withdraw  | ...      | 7      | 6               |    |

TLC をみる

一番上のテキストエリアに `Invaliant NoOverdrafts is violated.` てでてる

[Error-Trace] にそれを引き起こす状態遷移が書いてある

| Name                     | Value                   |  |
|--------------------------|-------------------------|--|
| ▼<Initial predicate>    | State (num=1)           |  |
| 　▶ acc                 | [alice ↦ 5, bob ↦ 5]  |  |
| 　　 amount              | 6                       |  |
| 　　 pc                  | "Withdraw"              |  |
| 　▶ people              | {"alice", "bob"}        |  |
| 　　reciever             | "bob"                   |  |
| 　　sender               | "alice"                 |  |
| ▼<Withdraw line 41, ... | State (num=2)           |  |
| 　▶ acc                 | [alice ↦ -1, bob ↦ 5] |!!|
| 　　 amount              | 6                       |  |
| 　　 pc                  | "Deposit"               |!!|
| 　▶ people              | {"alice", "bob"}        |  |
| 　　reciever             | "bob"                   |  |
| 　　sender               | "alice"                 |  |

> どうすれば修正できますか？
> 仕様を制限して、金額 `\in 1..acc[sender]` のみを考慮し、
> 合格とすることはできます.
>
> しかしながら、これは銀行の予想される仕様を反映していない可能性があります。
> 現実の世界の人々は、彼らがもっているよりも多くを移そうと試みるかもしれません。
> そして、我々はその場合に備えなければいけません。
>
> ただし、例を続けるために、これは許容可能な仮定とします。
> 変更をおこない、モデルが再び通過することを確認します。
