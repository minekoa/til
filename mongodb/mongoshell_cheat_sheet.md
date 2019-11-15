# mongo shell チートシート

https://qiita.com/rubytomato@github/items/390cdead677a851550e8

## show dbs / use <DBNAME> / show collections

```
> show dbs
admin         0.000GB
learn         0.000GB
local         0.035GB
test          0.000GB
tut_family_1  0.000GB
```

```
> use tut_family_1
switched to db tut_family_1
```

```
> show collections
sample
```

## db.<COLLECTIONNAME>.count()

```
> db.sample.count()
1
```

```
db.sample.find({})
{ "_id" : ObjectId("5c73a2969285a4002badd4b6"), "family_id" : ObjectId("5a0e81b6065a305566b5e4c5"), "creator_id" : ObjectId("5a0e81b6065a305566b5e4c8"), "content" : { "name" : "ContentName225", "category" : "CatDays" }, "created_at" : ISODate("2019-02-25T08:08:54.264Z"), "updated_at" : ISODate("2019-02-25T08:08:54.264Z") }
```
