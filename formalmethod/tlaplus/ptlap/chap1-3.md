# Practival TLA+ 学習メモ chap1 (その3)

## Multiple Processes

### まずは作成（そして仕様が壊れる）

`process`というブロックが`algorithm`のなかに作れるみたい

```
(*--algorithm wire

variables
    people = {"alice", "bob"},
    acc = [p \in people |-> 5],

define
    NoOverdrafts == \A p \in people: acc[p] >= 0
end define;

process Wire \in 1..2
    variables
        sender = "alice",
        receiver = "bob",
        amount \in 1..acc[sender];

begin
    Withdraw:
        acc[sender] := acc[sender] - amount;
    Deposit:
        acc[receiver] := acc[receiver] + amount;
end process;
end algorithm;*)
```

生成される TLCコードみると

```
ProcSet == (1..2)

Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        (* Process Wire *)
        /\ sender = [self \in 1..2 |-> "alice"]
        /\ receiver = [self \in 1..2 |-> "bob"]
        /\ amount \in [1..2 -> 1..acc[sender[CHOOSE self \in  1..2 : TRUE]]]
        /\ pc = [self \in ProcSet |-> "Withdraw"]

Withdraw(self) == /\ pc[self] = "Withdraw"
                  /\ acc' = [acc EXCEPT ![sender[self]] = acc[sender[self]] - amount[self]]
                  /\ pc' = [pc EXCEPT ![self] = "Deposit"]
                  /\ UNCHANGED << people, sender, receiver, amount >>

Deposit(self) == /\ pc[self] = "Deposit"
                 /\ acc' = [acc EXCEPT ![receiver[self]] = acc[receiver[self]] + amount[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << people, sender, receiver, amount >>

Wire(self) == Withdraw(self) \/ Deposit(self)
```

なんか OOPみたいになってきたな。
コンテキスト self をアクションが持つようになるのか。

でプロセス変数はマップに姿を変えるのか。

`self` is なんぞは

```
        /\ sender = [self \in 1..2 |-> "alice"]
```

```
        /\ pc = [self \in ProcSet |-> "Withdraw"]
```

のふた通りの書き方が生成されてて、若干キモい
（実は違いが在るのかな？）


さて、モデルチェックしてみると、エラー出るねって話。
グローバル変数に２つのプロセスがアクセスしてるんだからなー。

> これを再変換して再実行すると、再びエラーが発生します。
> 他のシステムが Aliceが6ドル wire しようとするのを停止している場合でも
> そのシステムは複数の配線では機能しません。
> その後、両方の withdraw が発生し、仕様が失敗します.

### 仕様を治す

```
(*--algorithm wire

variables
    people = {"alice", "bob"},
    acc = [p \in people |-> 5],

define
    NoOverdrafts == \A p \in people: acc[p] >= 0
end define;

process Wire \in 1..2
    variables
        sender = "alice",
        receiver = "bob",
        amount \in 1..acc[sender];

begin
    CheckFunds:
        if amount <= acc[sender] then
            Withdraw:
                acc[sender] := acc[sender] - amount;
            Deposit:
                acc[receiver] := acc[receiver] + amount;
        end if;
end process;
end algorithm;*)
```

CheckFunds: を追加した感じ。

これ、アクションを階層化させることができる？
意味論がわからない。

生成された TLCコードを見てみよう。

```
ARIABLES people, acc, pc

(* define statement *)
NoOverdrafts == \A p \in people: acc[p] >= 0

VARIABLES sender, receiver, amount

vars == << people, acc, pc, sender, receiver, amount >>

ProcSet == (1..2)

Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        (* Process Wire *)
        /\ sender = [self \in 1..2 |-> "alice"]
        /\ receiver = [self \in 1..2 |-> "bob"]
        /\ amount \in [1..2 -> 1..acc[sender[CHOOSE self \in  1..2 : TRUE]]]
        /\ pc = [self \in ProcSet |-> "CheckFunds"]

CheckFunds(self) == /\ pc[self] = "CheckFunds"
                    /\ IF amount[self] <= acc[sender[self]]
                          THEN /\ pc' = [pc EXCEPT ![self] = "Withdraw"]
                          ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                    /\ UNCHANGED << people, acc, sender, receiver, amount >>

Withdraw(self) == /\ pc[self] = "Withdraw"
                  /\ acc' = [acc EXCEPT ![sender[self]] = acc[sender[self]] - amount[self]]
                  /\ pc' = [pc EXCEPT ![self] = "Deposit"]
                  /\ UNCHANGED << people, sender, receiver, amount >>

Deposit(self) == /\ pc[self] = "Deposit"
                 /\ acc' = [acc EXCEPT ![receiver[self]] = acc[receiver[self]] + amount[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << people, sender, receiver, amount >>

Wire(self) == CheckFunds(self) \/ Withdraw(self) \/ Deposit(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in 1..2: Wire(self))
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")
```

あー、pc の遷移条件のみをいじるアクションか。
階層化は見せかけのものだね。


で、これもエラー。

| Name                | Value                          |    |
|---------------------|--------------------------------|----|
| <initial predicate> | State (num = 1)                |    |
| - acc               | [alice -> 5, bob -> 5]         |    |
| - amount            | <<1, 5>>                       |    |
| - pc                | <<"CheckFunds", "CheckFunds">> |    |
| - people            | {"alice", "bob"}               |    |
| - receiver          | <<"bob", "bob">>               |    |
| - sender            | <<"alice", "alice">>           |    |
| <CheckFunds ...     | State (num = 2)                |    |
| - acc               | [alice -> 5, bob -> 5]         |    |
| - amount            | <<1, 5>>                       |    |
| - pc                | <<"Withdraw", "CheckFunds">>   | !! |
| - people            | {"alice", "bob"}               |    |
| - receiver          | <<"bob", "bob">>               |    |
| - sender            | <<"alice", "alice">>           |    |
| <CheckFunds ...     | State (num = 3)                |    |
| - acc               | [alice -> 5, bob -> 5]         |    |
| - amount            | <<1, 5>>                       |    |
| - pc                | <<"Withdraw", "Withdraw">>     | !! |
| - people            | {"alice", "bob"}               |    |
| - receiver          | <<"bob", "bob">>               |    |
| - sender            | <<"alice", "alice">>           |    |
| <Withdraw ...       | State (num = 4)                |    |
| - acc               | [alice -> 4, bob -> 5]         | !! |
| - amount            | <<1, 5>>                       |    |
| - pc                | <<"Deposit", "Withdrow">>      | !! |
| - people            | {"alice", "bob"}               |    |
| - receiver          | <<"bob", "bob">>               |    |
| - sender            | <<"alice", "alice">>           |    |
| <Withdraw ...       | State (num = 5)                |    |
| - acc               | [alice -> -1, bob -> 5]        | !! |
| - amount            | <<1, 5>>                       |    |
| - pc                | <<"Deposit", "Deposit">>       | !! |
| - people            | {"alice", "bob"}               |    |
| - receiver          | <<"bob", "bob">>               |    |
| - sender            | <<"alice", "alice">>           |    |


なるほど、モデルチェック上 変数→プロセスでまとめたほうが見やすいのか。納得。

`!!` (実際には赤背景）は別にエラーというわけじゃなくって
変わった変数なのか

で流れを見れば、典型的な TAS の問題だね。

## TASの修正

```
    ・
    ・
process Wire \in 1..2
    variables
        sender = "alice",
        receiver = "bob",
        amount \in 1..acc[sender];

begin
    CheckAndWithdraw:
        if amount <= acc[sender] then
                acc[sender] := acc[sender] - amount;
            Deposit:
                acc[receiver] := acc[receiver] + amount;
        end if;
end process;
end algorithm;*)
```

CheckAndWithDraw として1状態遷移にした。

TLA+

```
Init == (* Global variables *)
        /\ people = {"alice", "bob"}
        /\ acc = [p \in people |-> 5]
        (* Process Wire *)
        /\ sender = [self \in 1..2 |-> "alice"]
        /\ receiver = [self \in 1..2 |-> "bob"]
        /\ amount \in [1..2 -> 1..acc[sender[CHOOSE self \in  1..2 : TRUE]]]
        /\ pc = [self \in ProcSet |-> "CheckAndWithdraw"]

CheckAndWithdraw(self) == /\ pc[self] = "CheckAndWithdraw"
                          /\ IF amount[self] <= acc[sender[self]]
                                THEN /\ acc' = [acc EXCEPT ![sender[self]] = acc[sender[self]] - amount[self]]
                                     /\ pc' = [pc EXCEPT ![self] = "Deposit"]
                                ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                                     /\ acc' = acc
                          /\ UNCHANGED << people, sender, receiver, amount >>

Deposit(self) == /\ pc[self] = "Deposit"
                 /\ acc' = [acc EXCEPT ![receiver[self]] = acc[receiver[self]] + amount[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << people, sender, receiver, amount >>

Wire(self) == CheckAndWithdraw(self) \/ Deposit(self)
```

当然モデルチェックは通る。
まぁ。そうなるよねってはなしでおしまい。
