## YAKML

## event

## external choise

csp:
```
Foo  = a -> Foo
    □ b -> Foo
    □ c -> Foo
```

kml:

```
state Foo {
    transiton a -> {
    }
    transiton b -> {
    }
    transiton c -> {
    }
```


## internal choise

csp:
```
Foo 0 = a -> ((Foo 1) ⊓ (Foo 2))
```

### plan A

kml:
```
state Foo {
    var v = 0

    transition a --> {
       internal_choise {
           post {
               target v
               v' = 1
           }
       }
       internal_choise {
           post {
               target v
               v' = 2
           }
       }
    }
}
```

### plan B

kml:

```
state Foo {
    var v = 0

    transition a --> {
       post {
           target v
           v' = 1
       }
       or post {
           target v
           v' = 2
       }
    }
}
```

* メリット
    * 自然な構造
    * インデントが一つ浅くなる
* デメリット
    * 内部選択自体のコメント（タイトル）をつける位置がない
        * { } @ {- -} 形式は同じもの、を強調するためなのでここにタイトルをつけるのはそぐわない
