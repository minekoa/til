# S2 更新

```
rs0:PRIMARY> db.unicorns.find({name: 'Roooooodles'})
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles", "dob" : ISODate("1979-08-18T09:44:00Z"), "loves" : [ "apple" ], "weight" : 575, "gender" : "m", "vampires" : 99 }
rs0:PRIMARY> db.unicorns.update({name: 'Roooooodles'}, {weight: 590})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.unicorns.find({name: 'Roooooodles'})
```
驚き！ find したとそのドキュメントをまるっと次のドキュメントで置き換える（該当フィールドのupdate構文ではない)

```
rs0:PRIMARY> db.unicorns.find( {weight: 590} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "weight" : 590 }
```

これになってる

レコードの更新には `$set` 演算子を使う

```
rs0:PRIMARY> db.unicorns.update( {weight: 590} , {$set: {name: 'Roooooodles', dob: new Date(1979, 7, 18, 18, 44), loves: ['apple'], gender: 'm', vampires: 99}})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.unicorns.find({name: 'Roooooodles'})
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "weight" : 590, "name" : "Roooooodles", "dob" : ISODate("1979-08-18T09:44:00Z"), "loves" : [ "apple" ], "gender" : "m", "vampires" : 99 }
```

## 2.2 更新演算子 11:43~

inc

```
rs0:PRIMARY> db.unicorns.find( {name: 'Pilot'} )
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 54 }
rs0:PRIMARY> db.unicorns.update({name: 'Pilot'}, {$inc: {vampires: -2}})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.unicorns.find( {name: 'Pilot'} )
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 52 }
```

push

```
rs0:PRIMARY> db.unicorns.find( {name: 'Aurora'} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "dob" : ISODate("1991-01-24T04:00:00Z"), "loves" : [ "carrot", "grape" ], "weight" : 450, "gender" : "f", "vampires" : 43 }
rs0:PRIMARY> db.unicorns.update({name: 'Aurora'}, {$push: {loves: 'sugar'}})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.unicorns.find( {name: 'Aurora'} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "dob" : ISODate("1991-01-24T04:00:00Z"), "loves" : [ "carrot", "grape", "sugar" ], "weight" : 450, "gender" : "f", "vampires" : 43 }
```

## 2.3 Upsert 11:54~

```
rs0:PRIMARY> db.hits.update({page: 'unicorns'}, {$inc: {hits: 1}});
WriteResult({ "nMatched" : 0, "nUpserted" : 0, "nModified" : 0 })
```


```
rs0:PRIMARY> show collections
unicorns
```

```
rs0:PRIMARY> db.hits.update({page: 'unicorns'}, {$inc: {hits: 1}}, {upsert: true});
WriteResult({
	"nMatched" : 0,
	"nUpserted" : 1,
	"nModified" : 0,
	"_id" : ObjectId("5c356332c1aafd1c78299a79")
})
```
```
rs0:PRIMARY> show collections
hits
unicorns
```

```
rs0:PRIMARY> db.hits.find( {page:'unicorns'} )
{ "_id" : ObjectId("5c356332c1aafd1c78299a79"), "page" : "unicorns", "hits" : 1 }
rs0:PRIMARY> db.hits.update({page: 'unicorns'}, {$inc: {hits: 1}}, {upsert: true});
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.hits.find( {page:'unicorns'} )
{ "_id" : ObjectId("5c356332c1aafd1c78299a79"), "page" : "unicorns", "hits" : 2 }
```

## 2.4 複数同時

```
rs0:PRIMARY> db.unicorns.update({}, {$set: {vaccinated: true }});
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
rs0:PRIMARY> db.unicorns.find({vaccinated: true});
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto", "gender" : "m", "home" : "Arrakeen", "worm" : false, "vaccinated" : true }
```

```
rs0:PRIMARY> db.unicorns.update({}, {$set: {vaccinated: true }}, {multi:true});
WriteResult({ "nMatched" : 13, "nUpserted" : 0, "nModified" : 12 })
rs0:PRIMARY> db.unicorns.find({vaccinated: true});
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto", "gender" : "m", "home" : "Arrakeen", "worm" : false, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc5"), "name" : "Horny", "dob" : ISODate("1992-03-12T22:47:00Z"), "loves" : [ "carrot", "papaya" ], "weight" : 600, "gender" : "m", "vampires" : 63, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "dob" : ISODate("1991-01-24T04:00:00Z"), "loves" : [ "carrot", "grape", "sugar" ], "weight" : 450, "gender" : "f", "vampires" : 43, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "weight" : 590, "name" : "Roooooodles", "dob" : ISODate("1979-08-18T09:44:00Z"), "loves" : [ "apple" ], "gender" : "m", "vampires" : 99, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "dob" : ISODate("1985-07-03T17:01:00Z"), "loves" : [ "apple", "carrot", "chocolate" ], "weight" : 550, "gender" : "f", "vampires" : 80, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh", "dob" : ISODate("2005-05-02T15:57:00Z"), "loves" : [ "apple", "sugar" ], "weight" : 421, "gender" : "m", "vampires" : 2, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "dob" : ISODate("2001-10-08T05:53:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 601, "gender" : "f", "vampires" : 33, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue", "dob" : ISODate("1999-12-20T07:15:00Z"), "loves" : [ "grape", "carrot" ], "weight" : 540, "gender" : "f", "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 52, "vaccinated" : true }
```
