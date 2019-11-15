# Erlang の Map

OTP17で後付された言語組み込みのデータ構造。結構異質。

[Erlang/OTP Map コトハジメ](https://gist.github.com/voluntas/7218024)

## 生成

```erlang
M0 = #{}
```

ハッシュマップなので `#` で修飾された `{}` と覚えると使いやすい？
（レコードと大混乱しそうな文法でもある）

```
#{ key => value } %% put
#{ key := value } %% update
```
