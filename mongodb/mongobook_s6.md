# 第6章 データの集計

## 6.1 アグリゲーション・パイプライン

```
rs0:PRIMARY> db.unicorns.aggregate( [{$group: {_id: '$gender', total: {$sum: 1}}}] )
{ "_id" : "f", "total" : 5 }
{ "_id" : "m", "total" : 8 }
```

フィールド名の前の$はドキュメントのフィールド値に置き換えられます。

### unwind

```
rs0:PRIMARY> db.unicorns.aggregate([{$unwind:'$loves'}, {$group: {_id:'$loves', total:{$sum:1}, unicorns:{$addToSet:'$name'}}}, {$sort:{total:-1}} ])
{ "_id" : "apple", "total" : 5, "unicorns" : [ "Pilot", "Roooooodles", "Solnara", "Leia", "Raleigh" ] }
{ "_id" : "grape", "total" : 4, "unicorns" : [ "Dunx", "Kenny", "Nimue", "Aurora" ] }
{ "_id" : "carrot", "total" : 4, "unicorns" : [ "Solnara", "Nimue", "Aurora", "Horny" ] }
{ "_id" : "watermelon", "total" : 3, "unicorns" : [ "Dunx", "Pilot", "Leia" ] }
{ "_id" : "lemon", "total" : 2, "unicorns" : [ "Kenny", "Ayna" ] }
{ "_id" : "sugar", "total" : 2, "unicorns" : [ "Raleigh", "Aurora" ] }
{ "_id" : "papaya", "total" : 1, "unicorns" : [ "Horny" ] }
{ "_id" : "energon", "total" : 1, "unicorns" : [ "Unicrom" ] }
{ "_id" : "redbull", "total" : 1, "unicorns" : [ "Unicrom" ] }
{ "_id" : "chocolate", "total" : 1, "unicorns" : [ "Solnara" ] }
{ "_id" : "strawberry", "total" : 1, "unicorns" : [ "Ayna" ] }
```

```
rs0:PRIMARY> db.unicorns.aggregate([{$group: {_id:'$loves', total:{$sum:1}, unicorns:{$addToSet:'$name'}}}, {$sort:{total:-1}} ])
{ "_id" : [ "apple", "watermelon" ], "total" : 2, "unicorns" : [ "Pilot", "Leia" ] }
{ "_id" : [ "grape", "watermelon" ], "total" : 1, "unicorns" : [ "Dunx" ] }
{ "_id" : [ "grape", "carrot" ], "total" : 1, "unicorns" : [ "Nimue" ] }
{ "_id" : [ "carrot", "papaya" ], "total" : 1, "unicorns" : [ "Horny" ] }
{ "_id" : [ "apple", "sugar" ], "total" : 1, "unicorns" : [ "Raleigh" ] }
{ "_id" : [ "strawberry", "lemon" ], "total" : 1, "unicorns" : [ "Ayna" ] }
{ "_id" : null, "total" : 1, "unicorns" : [ "Leto" ] }
{ "_id" : [ "apple" ], "total" : 1, "unicorns" : [ "Roooooodles" ] }
{ "_id" : [ "grape", "lemon" ], "total" : 1, "unicorns" : [ "Kenny" ] }
{ "_id" : [ "carrot", "grape", "sugar" ], "total" : 1, "unicorns" : [ "Aurora" ] }
{ "_id" : [ "energon", "redbull" ], "total" : 1, "unicorns" : [ "Unicrom" ] }
{ "_id" : [ "apple", "carrot", "chocolate" ], "total" : 1, "unicorns" : [ "Solnara" ] }
```

### 6.2 MapReduce

[Understanding Map Reduce](https://www.openmymind.net/2011/1/20/Understanding-Map-Reduce/)

を嫁とのこと。



