# CSP 文法めも

```
alphabet ::= [a-z][A-Za-z0-9_]*
process ::=  [A-z][A-Za-z0-9_]*
          | STOP
          | SKIP


complex_name ::= alphabet "." complex_name
               | alphabet

event ::= complex_name
        | channel "?" variables
        | channel "!" variables

event_set ::= "{" events "}"
events :=
        | event
        | event "," events

process_expression := process
                    | event "→" process_expression
                    | process_expression "□" process_expression
                    | process_expression "⊓" process_expression
                    | process_expression "|||" process_expression
                    | process_expression "|[" event_set "]|" process_expression
                    | process_expression "\" event_set
                    | process_expression ";" process_expression
```


```
LoginPage idtxt pwdtxt
    =  input_id_text s       → LoginPage(idtxt <- s)
    □ input_password_text s → LoginPage(pwdtxt <- s)
    □ authentication        → (MainPage ⊓ (LoginPage "" ""))
```
