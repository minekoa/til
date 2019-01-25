# ファイル名の頭にディレクトリ名をつける

環境としてはこんななんし

```make
OUTDIR=ebin
OBJS=hoge.beam piyo.beam fuga.beam
```

```make
$(OUTDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -o $(OUTDIR) $<
```

で、出力先フォルダにバイナリ(beam) がなかったらcompileしてね、と
依存関係を書きたい。
そのためには　`ebin/hoge.beam` みたいなリストに加工したい。

ネットでググると addprifix 使えとある

```make
compile: $(addprefix $(OUTDIR)/, $(OBJS))
```

でも、こんなふうにもかける

```make
compile: $(OBJS:%=$(OUTDIR)/%)
```

リスト内包的な書き方かな `[ outdir ++ "/" ++ fname | fname <- objs]`
応用が効きそうなので覚えておこう。
