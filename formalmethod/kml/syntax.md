# (WIP) syntax

思い出しながら、考えながら書いてる、最中。


## 値と型

```bnf
variable_name ::= [a-z][a-zA-Z0-9_]*[']?

lvalue ::= "(" lvalue ")"
         | variable_name
         | variable_name : type_anotation

lvalues ::= lvalue
          | lvalue "," lvalues

type_anotation ::= ":" type_instance

```

```ebnf
type_name ::= [A-Z][a-zA-Z0-9_]*
type_constructor ::= type_name

type_instance ::= "(" type_instance ")"
                | type_constructor
                | type_constructor type_rvalues

type_rvalues ::= type_instance
               | type_instance type_rvalues
```

## 式と述語

```
expression ::= "(" expression ")"
             | literal
             | variable_name
             | arithmetric_expression
             | function_call
             | set_expressoin
             | list_expression
             | map_expression
             | recode_expresion
             | process_expression
             | if_else_expression
             | let_expression
             | let_be_st_expression
             | predicate

predicate ::= logical_expression
            | comparison_expression
            | quantifier_expression
```

### 関数適用

```ebnf
function_call ::= variable_name
                | variable_name expression_list

expression_list ::= expression
                  | expression expression_list
```

### 比較演算

```ebnf
comparison_expression ::= expression "=" expression
                        | expression "!=" expressoin
                        | expression "<" expression
                        | expression "<=" expression
                        | expression ">" expression
                        | expression ">=" expression
```

### 論理演算

```ebnf
logical_expression ::= predicate "&&" predicate
                     | predicate "||" predicate
                     | predicate "=>" predicate
                     | predicate "xor" predicate
                     | "!" predicate
```

### 限量式

```
quantifier_expression ::= "forall" ..
                        | "exists" ..
                        | "exists!" ..
                        | expression "in" set_expression
```

(WIP)


### イベント (イベント式）

```ebnf
event_name ::= [a-z][a-zA-Z0-9_]*
channel_name ::= [a-z][a-zA-Z0-9_]*

event ::= event_name
        | event_name rvalues
        | channel "!" rvalues
        | channel "?" lvalues
        | "(" event ")"
```

イベントは、イベント名もしくは引数つきイベント名、
もしくはチャネル通信のいずれかである.

例

```
cancelButton_Clicked

nameInput_Inputted a_str

resultChan ! True

resuctChan ? isOk
```


### プロセス式

```ebnf
event_set ::= "{" events "}"
            | "{|" chan_or_events "|}"

events ::=
         | events_tail

events_tail ::= event
              | event "," events_tail



chan_or_events ::=
                 | chan_or_events_tail

chan_or_events_tail ::=
                      | chan_or_event
                      | chan_or_event "," chan_or_events_tail

chan_or_event ::= event
                | channel
```

```ebnf
process_expression ::= process
                    | "(" process_expression ")"
                    | event "→" process_expression
                    | process_expression "\" event_set
                    | process_expression "|[" event_set "]|" process_expression
                    | process_expression "|||" process_expression
                    | "STOP"
                    | "SKIP"
```


* 普通のプロセス
    * `P;Q` .. Pの後にQ
    * `P \ X` .. イベントの隠蔽
    * `ev -> P` .. ev が置きた後に Pに。
* 並行プロセス
    * `P ||| Q` ..インターリーブ。
    * `P |[ X ]| Q` インターフェイス並列
* 特殊
    * `STOP` .. 停止。
    * `SKIP` .. 正常終了。逐次合成したときには、こちらじゃないと次のプロセスに進行できない。
* `X` は、
    * `{ev1, ev2, ..}`
    * `{| chan1, chan2, ev1, chan3, ... |}`

### リテラル

```
literal ::= number_literal
          | chr_literal
          | tuple_literal
          | recode_riteral
          | set_riteral
          | list_literal
          | map_riteral
          | lambda_expression
```



### コンディション

```ebnf
condition_list ::=
                 | condition ";"
                 | condition ";" conditions

condition ::= "target" var_list
            | predicate
```

## 定義


### 型の定義

```ebnf
parametoric_type_name ::= type_name
                        | type_name type_lvalues

type_lvalues ::= type_lvalue
               | type_lvalue type_lvalues

type_lvalue  ::= ['][a-z][a-zA-Z0-9_]*

type_instance_or_arg ::= type_instance
                       | type_lvalue




type_def_statement ::= "type" parametoric_type_name "=" type_def_exp ";"

type_def_exp ::= "(" type_def_exp ")
               | variant_def
               | record_def


variant_deff ::= dadd_type_def

dadd_type_def ::= dsum_type_def
                | dsum_type_def "|" dadd_type_def

dsum_type_def ::= parametoric_type_name "of" dsum_type_def_tuple
                | type_name

dsum_type_def_tuple ::= type_instance_or_arg
                      | type_instance_or_arg "*" dsum_type_def_tuple


record_def ::= "{" rec_field_list "}"

rec_field ::= fieldName ":" type_expression
            | fieldName ":" record_def

rec_field_list ::= rec_field
                 | rec_field "," rec_field_list
```

例

```kml
type Hoge = Id of String;

type Point = Point of Int * Int

type OkCancel = OK
              | Cancel
              | ErrorList of List Int
;

type FullName = { lastName : String
                , familyName : String
                }
;

type Maybe 'a = Just of 'a
              | Nothing
;

type Hoge 'a = Result 'a of Maybe 'a
             | Info of String
;
```



## 変数の定義

```ebnf
variable_def_statement ::= "var" variable_name type_anotation varriable_initializer ";"

type_anotation ::=
                 | ":" type

varriable_initializer ::=
                        | "=" expression

variable_def_statement_list ::= variable_def_statement
                              | variable_def_statement variable_def_statement_list
```

例

```kml
var hogeHoge1 : String = "ほげほげ";

var hogeHoge2 = "ほげほげ";

var hogeHoge3 : String;
```



## state (プロセス定義）

```ebnf
process_name ::= [A-Z][a-zA-Z0-9_]*

process_lvalue ::= process_name
                 | process_name lvalues

state_def ::= "state" process_lvalue "{" state_body "}"
            | "state" process_lvalue "{" state_body "}" natural_spec_block

state_body ::= state_variables state_invariant state_transition_list

state_variables ::= variable_def_statement_list

state_invariant ::=
                  | "invariant" condition_block

state_transition_list ::= "transition" event_lvalue guead_phrase "-->" "{" post_condition "}"
                        | "transition" event_lvalue guead_phrase "-->" "{" post_condition "}" @ natural_spec_for_block

guead_phrase ::= "when" predicate natural_spec_for_phrase

post_condition ::= "post" condition_block

condition_block ::= "{" condition_list "}" natural_spec_for_block
```



## natural spec

```ebnf
natural_spec_for_block ::= "@" "{-" comment_string "-}"

natural_spec_for_expression ::= "@" "(-" comment_string "-)"

natural_spec_for_phrase ::= "@" "[-" comment_string "-]"

natural_spec_for_statement ::= "@" "--" comment_string crlf
```

(WIP) `comment_string` が手抜き
