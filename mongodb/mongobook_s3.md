# 第3章 検索の習得

## 3.1 フィールドの選択

```
rs0:PRIMARY> db.unicorns.find({}, {name: 1});
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia" }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue" }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx" }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot" }
```

```
rs0:PRIMARY> db.unicorns.find({}, {name: 1, _id: 0});
{ "name" : "Leto" }
{ "name" : "Horny" }
{ "name" : "Aurora" }
{ "name" : "Unicrom" }
{ "name" : "Roooooodles" }
{ "name" : "Solnara" }
{ "name" : "Ayna" }
{ "name" : "Kenny" }
{ "name" : "Raleigh" }
{ "name" : "Leia" }
{ "name" : "Nimue" }
{ "name" : "Dunx" }
{ "name" : "Pilot" }
```

## 3.2 順序

> これまでに何度か find が必要な時に遅延して実行されるカーソルを返すと説明しました。

そんな説明あったっけ？

```
> db.unicorns.find().sort( {weight: -1} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 52, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "dob" : ISODate("2001-10-08T05:53:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 601, "gender" : "f", "vampires" : 33, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny", "dob" : ISODate("1992-03-12T22:47:00Z"), "loves" : [ "carrot", "papaya" ], "weight" : 600, "gender" : "m", "vampires" : 63, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "weight" : 590, "name" : "Roooooodles", "dob" : ISODate("1979-08-18T09:44:00Z"), "loves" : [ "apple" ], "gender" : "m", "vampires" : 99, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "dob" : ISODate("1985-07-03T17:01:00Z"), "loves" : [ "apple", "carrot", "chocolate" ], "weight" : 550, "gender" : "f", "vampires" : 80, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue", "dob" : ISODate("1999-12-20T07:15:00Z"), "loves" : [ "grape", "carrot" ], "weight" : 540, "gender" : "f", "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "dob" : ISODate("1991-01-24T04:00:00Z"), "loves" : [ "carrot", "grape", "sugar" ], "weight" : 450, "gender" : "f", "vampires" : 43, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh", "dob" : ISODate("2005-05-02T15:57:00Z"), "loves" : [ "apple", "sugar" ], "weight" : 421, "gender" : "m", "vampires" : 2, "vaccinated" : true }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto", "gender" : "m", "home" : "Arrakeen", "worm" : false, "vaccinated" : true }
```

正格ではないということか。 sort |> find と実行されると理解すればいいのかな。

だとすると、
find の段階でソートキーのフィールドを除外しても正常にうごくのか

```
rs0:PRIMARY> db.unicorns.find({}, {name:1}).sort( {weight: -1} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna" }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny" }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara" }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora" }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh" }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto" }
```

期待通りに動く。


sort().find() してみる。多分動かない

```
rs0:PRIMARY> db.unicorns.sort( {weight: 1} ).find( {}, {name:1})
2019-01-09T12:17:52.833+0900 E QUERY    [thread1] TypeError: db.unicorns.sort is not a function :
@(shell):1:1
```

二項ソート。
```
rs0:PRIMARY> db.unicorns.find({},{name:1, vampires:1}).sort({name: 1, vampires: -1})
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "vampires" : 43 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "vampires" : 40 }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "vampires" : 165 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny", "vampires" : 63 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "vampires" : 39 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "vampires" : 33 }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto" }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue" }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "vampires" : 52 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh", "vampires" : 2 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles", "vampires" : 99 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "vampires" : 80 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "vampires" : 182 }
```
```
rs0:PRIMARY> db.unicorns.find({},{name:1, vampires:1}).sort({vampires: -1, name: 1})
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "vampires" : 182 }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "vampires" : 165 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles", "vampires" : 99 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "vampires" : 80 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny", "vampires" : 63 }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "vampires" : 52 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "vampires" : 43 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "vampires" : 40 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "vampires" : 39 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "vampires" : 33 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh", "vampires" : 2 }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto" }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue" }
```

フィールドの出現順にソート優先度が高くなる様子。


> リレーショナルデータベースと同様に、MongoDB もソートの為にインデックスを利用
> 出来ます。インデックスの詳細は後で詳しく見ていきますが、MongoDB にはインデック
> スを使用しない場合にソートのサイズ制限があることを知っておく必要があります。すな
> わち、もし巨大な結果に対してソートを行おうとするとエラーが返ってきます。

ほほー。


## 3.3 ページング

`limit`

```
rs0:PRIMARY> db.unicorns.find().sort({weight: -1}).limit(3)
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
rs0:PRIMARY> db.unicorns.find().sort({weight: -1}).limit(4)
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
```


`skip`

```
rs0:PRIMARY> db.unicorns.find().sort({weight: -1}).limit(4)
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
rs0:PRIMARY> db.unicorns.find().sort({weight: -1}).limit(4).skip(1)
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 52, "vaccinated" : true }
```

## 3.4 カウント


```
rs0:PRIMARY> db.unicorns.count({vampires: {$gt: 50}})
6
rs0:PRIMARY> db.unicorns.find({vampires: {$gt: 50}}).count()
6
```

上は、下のシェルシュガー
