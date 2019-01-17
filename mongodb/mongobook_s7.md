# 第7章 パフォーマンスとツール


## 7.1 インデックス

```
rs0:PRIMARY> db.unicorns.ensureIndex({name: 1});
{
	"createdCollectionAutomatically" : false,
	"numIndexesBefore" : 1,
	"numIndexesAfter" : 2,
	"ok" : 1
}
rs0:PRIMARY> db.unicorns.getIndexes()
[
	{
		"v" : 2,
		"key" : {
			"_id" : 1
		},
		"name" : "_id_",
		"ns" : "learn.unicorns"
	},
	{
		"v" : 2,
		"key" : {
			"name" : 1
		},
		"name" : "name_1",
		"ns" : "learn.unicorns"
	}
]
rs0:PRIMARY> db.unicorns.dropIndex({name: 1});
{ "nIndexesWas" : 2, "ok" : 1 }
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

ユニークインデックスを作る

```
rs0:PRIMARY> db.unicorns.ensureIndex({name: 1}, {unique: true});
{
	"createdCollectionAutomatically" : false,
	"numIndexesBefore" : 1,
	"numIndexesAfter" : 2,
	"ok" : 1
}
rs0:PRIMARY> db.unicorns.getIndexes()
[
	{
		"v" : 2,
		"key" : {
			"_id" : 1
		},
		"name" : "_id_",
		"ns" : "learn.unicorns"
	},
	{
		"v" : 2,
		"unique" : true,
		"key" : {
			"name" : 1
		},
		"name" : "name_1",
		"ns" : "learn.unicorns"
	}
]
```



複合キー


```
rs0:PRIMARY> db.unicorns.ensureIndex({name: 1, vampires: -1});
{
	"createdCollectionAutomatically" : false,
	"numIndexesBefore" : 2,
	"numIndexesAfter" : 3,
	"ok" : 1
}
rs0:PRIMARY> db.unicorns.getIndexes()
[
	{
		"v" : 2,
		"key" : {
			"_id" : 1
		},
		"name" : "_id_",
		"ns" : "learn.unicorns"
	},
	{
		"v" : 2,
		"unique" : true,
		"key" : {
			"name" : 1
		},
		"name" : "name_1",
		"ns" : "learn.unicorns"
	},
	{
		"v" : 2,
		"key" : {
			"name" : 1,
			"vampires" : -1
		},
		"name" : "name_1_vampires_-1",
		"ns" : "learn.unicorns"
	}
]
```

## 7.2 Explain

```
rs0:PRIMARY> db.unicorns.find().explain()
{
	"queryPlanner" : {
		"plannerVersion" : 1,
		"namespace" : "learn.unicorns",
		"indexFilterSet" : false,
		"parsedQuery" : {
			
		},
		"winningPlan" : {
			"stage" : "COLLSCAN",
			"direction" : "forward"
		},
		"rejectedPlans" : [ ]
	},
	"serverInfo" : {
		"host" : "3986556ba4d8",
		"port" : 27017,
		"version" : "3.4.13",
		"gitVersion" : "fbdef2ccc53e0fcc9afb570063633d992b2aae42"
	},
	"ok" : 1
}
```

```
rs0:PRIMARY> db.unicorns.find({name: 'Pilot'}).explain()
{
	"queryPlanner" : {
		"plannerVersion" : 1,
		"namespace" : "learn.unicorns",
		"indexFilterSet" : false,
		"parsedQuery" : {
			"name" : {
				"$eq" : "Pilot"
			}
		},
		"winningPlan" : {
			"stage" : "FETCH",
			"inputStage" : {
				"stage" : "IXSCAN",
				"keyPattern" : {
					"name" : 1
				},
				"indexName" : "name_1",
				"isMultiKey" : false,
				"multiKeyPaths" : {
					"name" : [ ]
				},
				"isUnique" : true,
				"isSparse" : false,
				"isPartial" : false,
				"indexVersion" : 2,
				"direction" : "forward",
				"indexBounds" : {
					"name" : [
						"[\"Pilot\", \"Pilot\"]"
					]
				}
			}
		},
		"rejectedPlans" : [
			{
				"stage" : "FETCH",
				"inputStage" : {
					"stage" : "IXSCAN",
					"keyPattern" : {
						"name" : 1,
						"vampires" : -1
					},
					"indexName" : "name_1_vampires_-1",
					"isMultiKey" : false,
					"multiKeyPaths" : {
						"name" : [ ],
						"vampires" : [ ]
					},
					"isUnique" : false,
					"isSparse" : false,
					"isPartial" : false,
					"indexVersion" : 2,
					"direction" : "forward",
					"indexBounds" : {
						"name" : [
							"[\"Pilot\", \"Pilot\"]"
						],
						"vampires" : [
							"[MaxKey, MinKey]"
						]
					}
				}
			}
		]
	},
	"serverInfo" : {
		"host" : "3986556ba4d8",
		"port" : 27017,
		"version" : "3.4.13",
		"gitVersion" : "fbdef2ccc53e0fcc9afb570063633d992b2aae42"
	},
	"ok" : 1
}
```

## 7.3 レプリケーション


## 7.5 統計

```
rs0:PRIMARY> db.stats()
{
	"db" : "learn",
	"collections" : 4,
	"views" : 0,
	"objects" : 20,
	"avgObjSize" : 125.85,
	"dataSize" : 2517,
	"storageSize" : 110592,
	"numExtents" : 0,
	"indexes" : 6,
	"indexSize" : 126976,
	"ok" : 1
}
```

```
rs0:PRIMARY> db.unicorns.stats()
{
	"ns" : "learn.unicorns",
	"size" : 1928,
	"count" : 13,
	"avgObjSize" : 148,
	"storageSize" : 36864,
	"capped" : false,
	"wiredTiger" : {
		"metadata" : {
			"formatVersion" : 1
		},
		"creationString" : "access_pattern_hint=none,allocation_size=4KB,app_metadata=(formatVersion=1),block_allocation=best,block_compressor=snappy,cache_resident=false,checksum=on,colgroups=,collator=,columns=,dictionary=0,encryption=(keyid=,name=),exclusive=false,extractor=,format=btree,huffman_key=,huffman_value=,ignore_in_memory_cache_size=false,immutable=false,internal_item_max=0,internal_key_max=0,internal_key_truncate=true,internal_page_max=4KB,key_format=q,key_gap=10,leaf_item_max=0,leaf_key_max=0,leaf_page_max=32KB,leaf_value_max=64MB,log=(enabled=true),lsm=(auto_throttle=true,bloom=true,bloom_bit_count=16,bloom_config=,bloom_hash_count=8,bloom_oldest=false,chunk_count_limit=0,chunk_max=5GB,chunk_size=10MB,merge_max=15,merge_min=0),memory_page_max=10m,os_cache_dirty_max=0,os_cache_max=0,prefix_compression=false,prefix_compression_min=4,source=,split_deepen_min_child=0,split_deepen_per_child=0,split_pct=90,type=file,value_format=u",
		"type" : "file",
		"uri" : "statistics:table:collection-0--1926938296917029593",
		"LSM" : {
			"bloom filter false positives" : 0,
			"bloom filter hits" : 0,
			"bloom filter misses" : 0,
			"bloom filter pages evicted from cache" : 0,
			"bloom filter pages read into cache" : 0,
			"bloom filters in the LSM tree" : 0,
			"chunks in the LSM tree" : 0,
			"highest merge generation in the LSM tree" : 0,
			"queries that could have benefited from a Bloom filter that did not exist" : 0,
			"sleep for LSM checkpoint throttle" : 0,
			"sleep for LSM merge throttle" : 0,
			"total size of bloom filters" : 0
		},
		"block-manager" : {
			"allocations requiring file extension" : 8,
			"blocks allocated" : 43,
			"blocks freed" : 10,
			"checkpoint size" : 4096,
			"file allocation unit size" : 4096,
			"file bytes available for reuse" : 16384,
			"file magic number" : 120897,
			"file major version number" : 1,
			"file size in bytes" : 36864,
			"minor version number" : 0
		},
		"btree" : {
			"btree checkpoint generation" : 430,
			"column-store fixed-size leaf pages" : 0,
			"column-store internal pages" : 0,
			"column-store variable-size RLE encoded values" : 0,
			"column-store variable-size deleted values" : 0,
			"column-store variable-size leaf pages" : 0,
			"fixed-record size" : 0,
			"maximum internal page key size" : 368,
			"maximum internal page size" : 4096,
			"maximum leaf page key size" : 2867,
			"maximum leaf page size" : 32768,
			"maximum leaf page value size" : 67108864,
			"maximum tree depth" : 3,
			"number of key/value pairs" : 0,
			"overflow pages" : 0,
			"pages rewritten by compaction" : 0,
			"row-store internal pages" : 0,
			"row-store leaf pages" : 0
		},
		"cache" : {
			"bytes currently in the cache" : 6418,
			"bytes read into cache" : 0,
			"bytes written from cache" : 17927,
			"checkpoint blocked page eviction" : 0,
			"data source pages selected for eviction unable to be evicted" : 0,
			"hazard pointer blocked page eviction" : 0,
			"in-memory page passed criteria to be split" : 0,
			"in-memory page splits" : 0,
			"internal pages evicted" : 0,
			"internal pages split during eviction" : 0,
			"leaf pages split during eviction" : 0,
			"modified pages evicted" : 0,
			"overflow pages read into cache" : 0,
			"overflow values cached in memory" : 0,
			"page split during eviction deepened the tree" : 0,
			"page written requiring lookaside records" : 0,
			"pages read into cache" : 0,
			"pages read into cache requiring lookaside entries" : 0,
			"pages requested from the cache" : 125,
			"pages written from cache" : 22,
			"pages written requiring in-memory restoration" : 0,
			"tracked dirty bytes in the cache" : 0,
			"unmodified pages evicted" : 0
		},
		"cache_walk" : {
			"Average difference between current eviction generation when the page was last considered" : 0,
			"Average on-disk page image size seen" : 0,
			"Clean pages currently in cache" : 0,
			"Current eviction generation" : 0,
			"Dirty pages currently in cache" : 0,
			"Entries in the root page" : 0,
			"Internal pages currently in cache" : 0,
			"Leaf pages currently in cache" : 0,
			"Maximum difference between current eviction generation when the page was last considered" : 0,
			"Maximum page size seen" : 0,
			"Minimum on-disk page image size seen" : 0,
			"On-disk page image sizes smaller than a single allocation unit" : 0,
			"Pages created in memory and never written" : 0,
			"Pages currently queued for eviction" : 0,
			"Pages that could not be queued for eviction" : 0,
			"Refs skipped during cache traversal" : 0,
			"Size of the root page" : 0,
			"Total number of pages currently in cache" : 0
		},
		"compression" : {
			"compressed pages read" : 0,
			"compressed pages written" : 0,
			"page written failed to compress" : 0,
			"page written was too small to compress" : 22,
			"raw compression call failed, additional data available" : 0,
			"raw compression call failed, no additional data available" : 0,
			"raw compression call succeeded" : 0
		},
		"cursor" : {
			"bulk-loaded cursor-insert calls" : 0,
			"create calls" : 12,
			"cursor-insert key and value bytes inserted" : 4331,
			"cursor-remove key bytes removed" : 1,
			"cursor-update value bytes updated" : 0,
			"insert calls" : 32,
			"next calls" : 637,
			"prev calls" : 1,
			"remove calls" : 1,
			"reset calls" : 110,
			"restarted searches" : 0,
			"search calls" : 24,
			"search near calls" : 19,
			"truncate calls" : 0,
			"update calls" : 0
		},
		"reconciliation" : {
			"dictionary matches" : 0,
			"fast-path pages deleted" : 0,
			"internal page key bytes discarded using suffix compression" : 0,
			"internal page multi-block writes" : 0,
			"internal-page overflow keys" : 0,
			"leaf page key bytes discarded using prefix compression" : 0,
			"leaf page multi-block writes" : 0,
			"leaf-page overflow keys" : 0,
			"maximum blocks required for a page" : 0,
			"overflow values written" : 0,
			"page checksum matches" : 0,
			"page reconciliation calls" : 22,
			"page reconciliation calls for eviction" : 0,
			"pages deleted" : 0
		},
		"session" : {
			"object compaction" : 0,
			"open cursor count" : 0
		},
		"transaction" : {
			"update conflicts" : 0
		}
	},
	"nindexes" : 3,
	"totalIndexSize" : 69632,
	"indexSizes" : {
		"_id_" : 36864,
		"name_1" : 16384,
		"name_1_vampires_-1" : 16384
	},
	"ok" : 1
}
```

## 7.7 プロファイラ


```
rs0:PRIMARY> db.setProfilingLevel(2);
{ "was" : 0, "slowms" : 100, "ok" : 1 }
```


```
rs0:PRIMARY> db.unicorns.find({weight: {$gt: 600}});
{ "_id" : ObjectId("5c3556ec93f5812a07d53bc7"), "name" : "Unicrom", "dob" : ISODate("1973-02-09T13:10:00Z"), "loves" : [ "energon", "redbull" ], "weight" : 984, "gender" : "m", "vampires" : 182, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bca"), "name" : "Ayna", "dob" : ISODate("1998-03-06T23:30:00Z"), "loves" : [ "strawberry", "lemon" ], "weight" : 733, "gender" : "f", "vampires" : 40, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcb"), "name" : "Kenny", "dob" : ISODate("1997-07-01T01:42:00Z"), "loves" : [ "grape", "lemon" ], "weight" : 690, "gender" : "m", "vampires" : 39, "vaccinated" : true }
{ "_id" : ObjectId("5c3556ec93f5812a07d53bcd"), "name" : "Leia", "dob" : ISODate("2001-10-08T05:53:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 601, "gender" : "f", "vampires" : 33, "vaccinated" : true }
{ "_id" : ObjectId("5c3556fe93f5812a07d53bcf"), "name" : "Dunx", "dob" : ISODate("1976-07-18T09:18:00Z"), "loves" : [ "grape", "watermelon" ], "weight" : 704, "gender" : "m", "vampires" : 165, "vaccinated" : true }
{ "_id" : ObjectId("5c35579893f5812a07d53bd0"), "name" : "Pilot", "dob" : ISODate("1997-02-28T20:03:00Z"), "loves" : [ "apple", "watermelon" ], "weight" : 650, "gender" : "m", "vampires" : 52, "vaccinated" : true }
```

```
rs0:PRIMARY> db.system.profile.find()
{ "op" : "query", "ns" : "learn.unicorns", "query" : { "find" : "unicorns", "filter" : { "weight" : { "$gt" : 600 } } }, "keysExamined" : 0, "docsExamined" : 13, "cursorExhausted" : true, "numYield" : 0, "locks" : { "Global" : { "acquireCount" : { "r" : NumberLong(2) } }, "Database" : { "acquireCount" : { "r" : NumberLong(1) } }, "Collection" : { "acquireCount" : { "r" : NumberLong(1) } } }, "nreturned" : 6, "responseLength" : 1028, "protocol" : "op_command", "millis" : 0, "planSummary" : "COLLSCAN", "execStats" : { "stage" : "COLLSCAN", "filter" : { "weight" : { "$gt" : 600 } }, "nReturned" : 6, "executionTimeMillisEstimate" : 0, "works" : 15, "advanced" : 6, "needTime" : 8, "needYield" : 0, "saveState" : 0, "restoreState" : 0, "isEOF" : 1, "invalidates" : 0, "direction" : "forward", "docsExamined" : 13 }, "ts" : ISODate("2019-01-09T08:10:55.149Z"), "client" : "172.18.0.4", "appName" : "MongoDB Shell", "allUsers" : [ { "user" : "admin", "db" : "admin" } ], "user" : "admin@admin" }
```
