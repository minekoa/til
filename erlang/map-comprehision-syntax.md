# Map の 内包表記 (EEP43)

すごいE本の付録Bにマップについての記述がある.
研修読本としてこれをつかっていたのだけれども、Map内包表記が動かないというヘルプが来て
はじめてそんなもののの存在を知ったのだった。

## EEP43

[EEP 43: Maps](rlang.org/eeps/eep-0043.html) によると、文法はこんな感じ

> ## Map comprehension syntax
>
> Map comprehension declaration:
>
> ```erlang
> M1 = #{ E0 => E1 || K := V <- M0  }
> ```
>
> where `M0` is any Map, `E0` and `E1` are any erlang expression, `K`
> and `V` constitutes the pattern to be matched by each association in `M0`.
>
> For each sequence in the generator an association is created from the
> evaluated expression `E0` to the evaluated expression `E1`.
>
> If `M0` is not a Map, then a runtime exception of type
> `{bad_generator, M0}` will be generated.

> Examples:
>
> ```erlang
> M0 = #{ K => V*2  || K := V <- map() },
> M1 = #{ I => f(I) || I <- list() },
> M2 = #{ K => V    || <<L:8,K:L/binary,V/float>> <= binary() }.
> ```

で、結論から言うと、これは採用されていない。

EEP は Erlang Enhancement Proposal (Erlang 拡張提案) の略。
すごいE本を読んでると、あたかも新しい Erlang に取り込まれることが確定したかのように
よめてしまうけれど、項のタイトルが EEP!EEP! とかになってて、
いやらしい。
