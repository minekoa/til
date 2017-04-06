# Erlangの listsモジュールの Tips

* [Erlangのlistsモジュールを試してみる -第2回 | mike、mikeなるままに](http://mike-neck.github.io/blog/2013/05/14/erlangfalselistsmoziyuruwoshi-sitemiru-di-2hui/)

## keyfind/3

```
Format:
  keyfind(Key,N,TupleList) -> Tuple | false

Types:
  Key = term()
  N   = integer() = 1..tuple_size(Tuple)
  TupleList = [Tuple]
  Tuple = tuple()
```

タプルのリストから、指定位置の要素について `Key` に一致する最初のタプルを返す。

```erlang
TargetList = [{1, habit, noun}, {2, hail, noun}, {3, handle, verb}, {4, happy, adj}],
lists:keyfind( handler, 2, TargetList ).
```

