# 文法案

## 大きな構造

```ebnf
State ::= "state" TypeName "@" "(-" CommentString "-)" "{" StateBody "}:

StateBody ::= StateElm
            | StateElm StateBody

StateElm ::= Invariant | Transition

Invariant ::= "invariant" "{" ExpressionList "}" "@" "{-" CommentString "-}"
            | "invariant" "{-" CommentString "-}" "@" "{" ExpressionList "}"

Transition ::= "transition" EventExpression (Guird)? "-->" (TransitionBody | InternalChoiceList)
```

```
state FooState @ (- fooという大きな状態 -) {
    invaliant {
         ...
    } @ {-
       不変条件のコメント
   -}

   transition eventFoo n
         when [ ... ] @ [- ガードのコメント -]
   --> {
       post {
       } @ {-
           事後条件のコメント
       -}
   } @ {-
   -}
}
```

## イベント式

```ebnf
EventExpression ::= Symbol
                  | Symbol VariableList
                  | ChannelExpression
```

## ガード句

```ebnf
Guird ::= "when" "[" Expression "]" "@" "[-" CommentString "-]"
```

## 状態遷移

``` ebnf
TransitionBody ::= "{" PostCondition "}"
                 | "{" PostCondition "}" "@" "{-" CommentString "-}"
                 | "{-" CommentString "-}" "@" "{" PostCondition "}"
```

### 事後条件

```ebnf
PostCondition ::= "post" "{" (TargetExpression)? ExpressionList "}" "@" "{-" CommentString "-}"
                | "post" "{-" CommentString "-}" "@" "{" (TargetExpression)? ExpressionList "}"

TargetExpression ::= "target" VariableList
```

### 内部選択

```ebnf
InternalChoiceList ::= InternalChoice
                     | InternalChoice InternalChoiceList

InternalChoice ::= "internal_choice" TransitionBody
                 | "internal_choise" "@" "[-" CommentString "-]" TransitionBody
```

VariableList ::=
Expression ::=
