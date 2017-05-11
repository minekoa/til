Definitions.

WHITESPACE = [\s\t]
CR         = [\n]
WORD       = [A-Za-z0-9_]+

Rules.

{WORD}         : {token, {ident   , TokenLine, TokenChars}}.
{CR}           : {token, {cr      , TokenLine}}.

\.             : {token, {'.'     , TokenLine}}.
\;             : {token, {';'     , TokenLine}}. 
\+\-           : {token, {'+-'    , TokenLine}}.
\|\-           : {token, {'|-'    , TokenLine}}.
\\\-           : {token, {'\\-'   , TokenLine}}.
\|             : {token, {'|'     , TokenLine}}.
\-             : {token, {'-'     , TokenLine}}.

{WHITESPACE}+  : skip_token.

Erlang code.
