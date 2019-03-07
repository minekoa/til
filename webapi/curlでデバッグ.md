# curlコマンドでREST APIをデバッグ

## 例

### 作成 (POST)

```
curl -i -X POST -s http://localhost:8080/tutorial/5a0e81b6065a305566b5e4c7/note  -d '{"title": "mynote01", "body":"this is my fst note."}' -H "Content-Type:application/json" -H "Authorization:x-ridill tq8A5msfbPAA85A82vPQXmX16POGby2J"

HTTP/1.1 201 Created
cache-control: no-cache
content-length: 0
date: Wed, 06 Mar 2019 05:00:47 GMT
location: /tutorial/note/5c7f53ff71a6b80958466944
server: Cowboy
```

### 取得 (GET)

```
curl -i -X GET -s http://localhost:8080/tutorial/note/5c7f53ff71a6b80958466944  -H "Content-Type:application/json" -H "Authorization:x-ridill tq8A5msfbPAA85A82vPQXmX16POGby2J"
HTTP/1.1 200 OK
cache-control: no-cache
content-length: 174
content-type: application/json
date: Wed, 06 Mar 2019 05:02:08 GMT
etag: "2C7456BC"
server: Cowboy

{"auther":"5a0e81b6065a305566b5e4c8","body":"this is my fst note.","createdAt":"2019-03-06T14:00:47.923+09:00","title":"mynote01","updatedAt":"2019-03-06T14:00:47.923+09:00"}
```

## オプションの説明

| option      | description                  |
|-------------|------------------------------|
| `-i`        | ヘッダもボディも出力するよ   |
| `-X` METHOD | メソッドを指定するよ         |
| `-s` URL    | URLを指定するよ              |
| `-d` STRING | ポストするボディだよ         |
| `-H` STRING | ヘッダだよ。複数つけられるよ |

## リンク

* [curl コマンド 使い方メモ | Qiita](https://qiita.com/yasuhiroki/items/a569d3371a66e365316f)
* [curlでGET/POST/PUT/DELETE |やるしかなっちゃん](http://k213.hatenadiary.jp/entry/2016/03/30/010924)
* [curlでJSONをPOSTする | Qiita](https://qiita.com/moru3/items/0d429b1169befcaf2deb)
* [curlコマンドの使い方15選](https://news.mynavi.jp/article/20180821-680632/)
* [curlコマンドでHTTPヘッダを扱うレシピ | Qiita](https://qiita.com/youcune/items/45271a32dccb7498033f)
* [man](http://zaurus.catstar.org/archive/curl_manual_ja.html)
* [(対処法) 415 Unsupported media type](http://www.bmoo.net/archives/2013/10/315103.html)
