# 文法案

## 大きな構造

```ebnf
State ::= "state" TypeName "@" "(-" CommentString "-)" "{" StateBody "}:

StateBody ::= StateElm
            | StateElm StateBody

StateElm ::= StateVarDefs | Invariant | Transition

StateVarDefs ::= StateVarDef
               | StateVarDef StateVarDefs

Invariant ::= "invariant" "{" ExpressionList "}" "@" "{-" CommentString "-}"
            | "invariant" "{-" CommentString "-}" "@" "{" ExpressionList "}"

Transition ::= "transition" EventExpression (Guird)? "-->" (TransitionBody | InternalChoiceList)
```

```
state FooState @ (- fooという大きな状態 -) {
    var Hoge : Int = 1;

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

ChannelExpression ::= Symbol ! Expression
                    | Symbol ! Pattern
```

## ガード句

```ebnf
Guird ::= "when" "[" Expression "]" "@" "[-" CommentSring "-]"
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

導入するかは検討中.

```ebnf
InternalChoiceList ::= InternalChoice
                     | InternalChoice InternalChoiceList

InternalChoice ::= "internal_choice" TransitionBody
                 | "internal_choise" "@" "[-" CommentString "-]" TransitionBody
```

VariableList ::=
Expression ::=

## 型

```ebnf
TypeSignatue ::= TypeName
               | TypeName TypeArgs

TypeArgs  ::= TypeArg | TypeArg TypeArgs

TypeName ::= [A-Z][A-Za-z0-9_]*
TypeArg  ::= [a-z][A-Za-z0-9_]*
```

```
TypeDefinition ::= TypeName (TypeList)?
```

### タプル

### レコード

```ebnf
RecodeDef ::= "type" "alias" TypeSignature "=" RecodeLit ";"

```

### 代数的データ型

```ebnf
VariantDef ::= "type" TypeSignatue "=" VariantDefBody

VariantDefBody ::= VariantElem
                 | VariantElem "|" VariantList

VariantElem ::= TypeSignatue
              | TypeSignatue TypeSigList

TypeSigList ::= TypeSignatue
              | TypeSignatue TypeSigList
```

## 文

```
Statement ::= TypeDefinition
            | VariableDefinition

StatementTerm ::= ";"
                | ";" "@" LineComment
```

```
Block ::= "{" Statements "}" BlockTerm

BlockTerm ::= "}"
            | "}" "@" BlockComment
```

## コメント

```
LineComment  ::= "--" (RawString not contain <cr>) <cr>

BlockComment ::= "{-" (RawString not contain "-}") "-}"
```

## パターン (lvalue)

```
Pattern ::= VariableSig
          | TypeLitteral

VariableSig ::= "(" VariableSig ")"
              | Symbol
              | Symbol TypeAnnotation

TypeLitteral ::= Variant
```
