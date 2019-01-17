# 第4章 データモデリング


## 4.1 Join がありません

従業員(employees) コレクションを作成

```
rs0:PRIMARY> db.employees.insert({_id: ObjectId("4d85c7039ab0fd70a117d730"), name: 'Leto'})
WriteResult({ "nInserted" : 1 })
rs0:PRIMARY> db.employees.find()
{ "_id" : ObjectId("4d85c7039ab0fd70a117d730"), "name" : "Leto" }
rs0:PRIMARY> db.employees.insert({_id: ObjectId("4d85c7039ab0fd70a117d731"), name: 'Duncan',
... manager: ObjectId("4d85c7039ab0fd70a117d730")});
WriteResult({ "nInserted" : 1 })
rs0:PRIMARY> db.employees.insert({_id: ObjectId("4d85c7039ab0fd70a117d732"), name: 'Moneo',
... manager: ObjectId("4d85c7039ab0fd70a117d730")});
WriteResult({ "nInserted" : 1 })
```

```
rs0:PRIMARY> db.employees.find({}, {name: 1})
{ "_id" : ObjectId("4d85c7039ab0fd70a117d730"), "name" : "Leto" }
{ "_id" : ObjectId("4d85c7039ab0fd70a117d731"), "name" : "Duncan" }
{ "_id" : ObjectId("4d85c7039ab0fd70a117d732"), "name" : "Moneo" }
```

マネージャで検索

```
rs0:PRIMARY> db.employees.find({manager: ObjectId("4d85c7039ab0fd70a117d730")})
{ "_id" : ObjectId("4d85c7039ab0fd70a117d731"), "name" : "Duncan", "manager" : ObjectId("4d85c7039ab0fd70a117d730") }
{ "_id" : ObjectId("4d85c7039ab0fd70a117d732"), "name" : "Moneo", "manager" : ObjectId("4d85c7039ab0fd70a117d730") }
```
### 4.1.1 配列と埋め込みドキュメント

複数のマネージャを持つ場合、フィールドがヴェクタであればいい

```
rs0:PRIMARY> db.employees.insert({_id: ObjectId("4d85c7039ab0fd70a117d733"), name: 'Siona',
... manager: [ObjectId("4d85c7039ab0fd70a117d730"),
... ObjectId("4d85c7039ab0fd70a117d732")]})
WriteResult({ "nInserted" : 1 })
rs0:PRIMARY> db.employees.find({manager: ObjectId("4d85c7039ab0fd70a117d730")})
{ "_id" : ObjectId("4d85c7039ab0fd70a117d731"), "name" : "Duncan", "manager" : ObjectId("4d85c7039ab0fd70a117d730") }
{ "_id" : ObjectId("4d85c7039ab0fd70a117d732"), "name" : "Moneo", "manager" : ObjectId("4d85c7039ab0fd70a117d730") }
{ "_id" : ObjectId("4d85c7039ab0fd70a117d733"), "name" : "Siona", "manager" : [ ObjectId("4d85c7039ab0fd70a117d730"), ObjectId("4d85c7039ab0fd70a117d732") ] }
```

フィールドにドキュメントを持つこともできる。この場合findは `family.mother` のように `.` 連結で記述する。

```
rs0:PRIMARY> db.employees.insert({_id: ObjectId("4d85c7039ab0fd70a117d734"), name: 'Ghanima',
... family: {mother: 'Chani',
... father: 'Paul',
... brother: ObjectId("4d85c7039ab0fd70a117d730")}})
WriteResult({ "nInserted" : 1 })
rs0:PRIMARY> db.employees.find({'family.mother': 'Chani'})
{ "_id" : ObjectId("4d85c7039ab0fd70a117d734"), "name" : "Ghanima", "family" : { "mother" : "Chani", "father" : "Paul", "brother" : ObjectId("4d85c7039ab0fd70a117d730") } }
```

```
rs0:PRIMARY> db.employees.insert({_id: ObjectId(
... "4d85c7039ab0fd70a117d735"),
... name: 'Chani',
... family: [ {relation:'mother',name: 'Chani'},
... {relation:'father',name: 'Paul'},
... {relation:'brother', name: 'Duncan'}]})
WriteResult({ "nInserted" : 1 })
```

```
rs0:PRIMARY> db.employees.find( {family: {relation: 'father', name: 'Paul' } } )
{ "_id" : ObjectId("4d85c7039ab0fd70a117d735"), "name" : "Chani", "family" : [ { "relation" : "mother", "name" : "Chani" }, { "relation" : "father", "name" : "Paul" }, { "relation" : "brother", "name" : "Duncan" } ] }
```

### 4.1.2 非正規化

ブログのコンテンツの投稿者。
ユーザーIDだけ持たすのではなく、表示するなら名前も持たせてしまえという話。


ブログのコメントも、コンテンツと別にもつより
埋め込みドキュメントで持ってしまったほうが良いと示唆
（1ドキュメント16MB制限に引っかからないんじゃないかな、ハムレットも 200Kしかないし・・・的な話があった）
