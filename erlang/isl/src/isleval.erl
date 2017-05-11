-module(isleval).
-export([from_string/1, from_file/1]).

from_string(String) ->
    Ast       = parse_string(String),
    eval(Ast, []).

from_file( FileName ) ->
    {ok, F } = file:read_file(FileName),
    from_string(binary_to_list(F)).

parse_string(String) ->
    {ok, Tokens, _} = islscanner:string(String),
    {ok, Ast}       = islparser:parse(Tokens),
    Ast.

eval([], Es)->
    lists:reverse(Es);
eval([H|T], Es) ->    
    Es2 = eval(H, Es),
    eval(T, Es2);
eval( {statement, L, N1}, Es) ->
    {Es2, _, _, _} = eval(N1, Es, none, [], 1),
    Es2.


eval( {'-', _, N1, N2}, {Edges, Vports} ) ->
    {Lp, Eds2, Vps2} = eval(N1, {Edges, Vports} ),
    Rp = lport(N2),
    {rport(N2), add_edge(Eds2, Lp, Rp), Vps2};

eval( {'+-', _, N1, N2}, {Edges, Vports} ) ->
    {Lp, Eds2, Vps2} = eval(N1, {Edges, Vports} ),
    Rp = lport(N2),
    {rport(N2), add_edge(Eds2, Lp, Rp), add_vport(Vps2, Lp)};

eval ({V, Ln, N1, N2},  {Edges, Vports}) ->
    {Eds2, Vps2} = eval(N1, {Edges, Vports}),

    {Vope, Offset} = vope(V, 0),
    Vps3 = vports_skip(Vps2, Offset),
    eval({Vope, Ln, N2}, {Eds2, Vps3});

eval ({'|-', _, N1}, {Edges, Vports}) ->
    Lp = vports_get(Vports),
    Rp = lport
    .
eval ({'\-', _, N1}, {Edges, Vports}) ->
    .

    




vope({vope, _, '|' , Vope}, I) -> vope(Vope, I+1);
vope({vope, _, '|-'      }, I) -> {'|-', I};
vope({vope, _, '\\-'     }, I) -> {'\\-', I}.
    


%% 回路情報編集
add_edge( Edges, Lp, Rp ) ->
    [{edge, Lp, Rp} | Edges].

%% vports: 分岐結線式を解釈するための一時オブジェクト
%%
%%  Vports  .. +- 演算子で作られた結節点のリスト。逆順。
%%  Offset  .. 現在アクティブになっている結節点(0 << len)
vports_push( {Vports, Offset}, NewPort ) ->
    Vps2 = lists:sublist(Vports, Offset).
    {[NewPort|Vps2], Offset +1}.

vports_get( {Vports, Offset} )->
    {lists:nth(Offset, Vports), {Vports, Offset -1}}.

vports_pop( {Vports, Offset} ) ->
    Vps2 = lists:sublist(Vport, Offset -1) ++ lists:nthtail(Offset, Vports),
    {lists:nth(Offset, Vports), {Vps2, Offset -1}}.

vports_skip( {Vports, Offset}, SkipCount ) ->
    {Vports, Offset - SkipCount}.

vports_reset( {Vports, Offset} ) ->
    {Vports, length(Vport)}.

%% device タプルから ポートを生成する
%%   タプルのどの要素がpinでどの要素がチップなのか構文上、一意に定まらない。
%%   右または左のポートとして取り出すときに確定する
rport( {device, L, N1, none, none} ) -> {rport, {pin, N1}, {chip, none}};
rport( {device, L, N1, N2, none} )   -> {rport, {pin, N2}, {chip, N1}};
rport( {device, L, N1, N2, N3} )     -> {rport, {pin, N3}, {chip, N2}}.
    
lport( {device, L, N1, N2, _} )      -> {lport, {pin, N1}, {chip, N2}}.

vope_indent( {vope, _, '|', Op}, I } -> vope_indent(Op, I + 1);
vope_indent( {vope, _, Op}, I )      -> {Op, I}.


