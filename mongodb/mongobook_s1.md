# 第一章基礎

show dbs
インスタンス内のデータベースをリスト

```
rs0:PRIMARY> show dbs
admin         0.000GB
cfc_family_1  0.000GB
cfc_master    0.000GB
enq_family_1  0.000GB
enq_master    0.005GB
local         0.013GB
rdl_family_1  0.000GB
rdl_master    0.001GB
tut_family_1  0.000GB
wdb_family_1  0.000GB
```


mongo の Shell は JavaScript。


```
rs0:PRIMARY> use learn
switched to db learn
```

```
rs0:PRIMARY> db.unicorns.insert({name: 'Aurora', gender: 'f', weight: 450} )
WriteResult({ "nInserted" : 1 })
```

```
rs0:PRIMARY> db.unicorns.find()
{ "_id" : ObjectId("5c3554cd93f5812a07d53bc3"), "name" : "Aurora", "gender" : "f", "weight" : 450 }
```

```
rs0:PRIMARY> db.unicorns.getIndexes()
[
	{
		"v" : 2,
		"key" : {
			"_id" : 1
		},
		"name" : "_id_",
		"ns" : "learn.unicorns"
	}
]
```
Index 何？


```
rs0:PRIMARY> db.unicorns.insert({name: 'Leto', gender: 'm', home: 'Arrakeen', worm: false})
WriteResult({ "nInserted" : 1 })
rs0:PRIMARY> db.unicorns.find()
{ "_id" : ObjectId("5c3554cd93f5812a07d53bc3"), "name" : "Aurora", "gender" : "f", "weight" : 450 }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto", "gender" : "m", "home" : "Arrakeen", "worm" : false }
```


## 1.1 セレクタの習得 11:03~


s0:PRIMARY> db.unicorns.find({gender:'m', weight: {$gt: 700}})
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182 }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165 }
rs0:PRIMARY> 
rs0:PRIMARY> 
rs0:PRIMARY> db.unicorns.find({vampires: {$exists: false}})
{ "_id" : ObjectId("5c3554cd93f5812a07d53bc3"), "name" : "Aurora", "gender" : "f", "weight" : 450 }
{ "_id" : ObjectId("5c3555df93f5812a07d53bc4"), "name" : "Leto", "gender" : "m", "home" : "Arrakeen", "worm" : false }
{ "_id" : ObjectId("5c3556fa93f5812a07d53bce"), "name" : "Nimue", "dob" : ISODate("1999-12-20T07:15:00Z"), "loves" : [ "grape", "carrot" ], "weight" : 540, "gender" : "f" }
rs0:PRIMARY> 
rs0:PRIMARY> 
rs0:PRIMARY> db.unicorns.find( {loves: {$in: ['apple', 'orange']}} )
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc8"), "name" : "Roooooodles", "dob" : ISODate("1979-08-18T09:44:00Z"), "loves" : [ "apple" ], "weight" : 575, "gender" : "m", "vampires" : 99 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "dob" : ISODate("1985-07-03T17:01:00Z"), "loves" : [ "apple", "carrot", "chocolate" ], "weight" : 550, "gender" : "f", "vampires" : 80 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcc"), "name" : "Raleigh", "dob" : ISODate("2005-05-02T15:57:00Z"), "loves" : [ "apple", "sugar" ], "weight" : 421, "gender" : "m", "vampires" : 2 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "dob" : ISODate("2001-10-08T05:53:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 601, "gender" : "f", "vampires" : 33 }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 54 }
rs0:PRIMARY> db.unicorns.find( {gender: 'f', $or: [{loves: 'apple'}, {weight: {$lt: 500}}]})
{ "_id" : ObjectId("5c3554cd93f5812a07d53bc3"), "name" : "Aurora", "gender" : "f", "weight" : 450 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc6"), "name" : "Aurora", "dob" : ISODate("1991-01-24T04:00:00Z"), "loves" : [ "carrot", "grape" ], "weight" : 450, "gender" : "f", "vampires" : 43 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc9"), "name" : "Solnara", "dob" : ISODate("1985-07-03T17:01:00Z"), "loves" : [ "apple", "carrot", "chocolate" ], "weight" : 550, "gender" : "f", "vampires" : 80 }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "dob" : ISODate("2001-10-08T05:53:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 601, "gender" : "f", "vampires" : 33 }
rs0:PRIMARY> db.unicorns.find({_id: ObjectId("TheObjectId")})
2019-01-09T11:15:59.047+0900 E QUERY    [thread1] Error: invalid object id: length :
@(shell):1:24
rs0:PRIMARY> db.unicorns.find({_id: ObjectId("5c3554cd93f5812a07d53bc3")})
{ "_id" : ObjectId("5c3554cd93f5812a07d53bc3"), "name" : "Aurora", "gender" : "f", "weight" : 450 }
rs0:PRIMARY> 


S1おしまい 
