# 出力先ディレクトリがなかったら自動で作らせたい

```make
$(OUTDIR)/%.beam: $(SRCDIR)/%.erl | $(OUTDIR)

$(OUTDIR):
	mkdir $@
```

`$(OUTDIR)` そのものへ依存させて、生成ルールに makedir を入れれば良い.
言われてみれば単純明快。
