Nonterminals
program stmlist stm line exp hpath vpath vope device
.

Terminals
ident cr
'+-' '|' '|-' '\\-' '.' ';' '-'
.

Left  300 '+-' '-'. 
Left  400 '|' '|-' '\\-'. 
Left  600 '.'.

Rootsymbol program.

program -> stmlist         : '$1'.

stmlist -> stm             : ['$1'].
stmlist -> stm stmlist     : ['$1'] ++ '$2'.

stm -> exp ';' cr          : {statement, line_of('$2'), '$1'}.
stm -> line ';'            : {statement, line_of('$2'), '$1'}.
     
line -> exp cr             : {line, line_of('$2'), '$1'}.

exp -> hpath               : '$1'.
exp -> vpath               : '$1'.   

hpath -> device            : '$1'.
hpath -> hpath '-'  hpath  : {'-'    , line_of('$2'), '$1', '$3'}.
hpath -> hpath '+-' hpath  : {'+-'   , line_of('$2'), '$1', '$3'}.

vpath -> line  vope hpath  : {vpath, line_of('$2'), '$2', '$1', '$3'}.
vpath -> vpath vope hpath  : {vpath, line_of('$2'), '$2', '$1', '$3'}.

vope -> '|-'               : {vope, line_of('$1'), '|-'     }.
vope -> '\\-'              : {vope, line_of('$1'), '\\-'    }.
vope -> '|' vope           : {vope, line_of('$1'), '|', '$2'}.

device -> ident '.' ident '.' ident  : {device, line_of('$2'), value_of('$1'), value_of('$3'), value_of('$5')}.
device -> ident '.' ident            : {device, line_of('$2'), value_of('$1'), value_of('$3'), none}.
device -> ident                      : {device, line_of('$1'), value_of('$1'), none, none}.

Erlang code.

line_of(Token) ->
    element(2, Token).

value_of(Token) ->
    element(3, Token).


