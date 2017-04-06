# WebAPI についていろいろ調べてみた

例えばロボットを動かすような実世界のプログラミングにおいて、どのようなWebAPIが良いのだろうか。


## まずは、WebAPIの種類について考えてみよう。

[WEB Api の話 (Restful)](http://qiita.com/xuj/items/7e9501f91df9eb9e23bf)

上記エントリーから引用すると、以下のよう感じになる

|API 名	    | プロトコル | データの形式	|通信  |
|-----------|------------|--------------|------|
|REST	    | REST	     | JSON、XML	|同期  |
|SOAP	    | SOAP	     | XML	        |同期  |
|Streaming	| Bayeux	 | JSON	        |非同期|
|XML-RPC	| RPC	     | XML	        |同期  |
|JSON-RPC	| RPC	     | JSON         |非同期|

この通信の「同期」「非同期」とはなんのことだろうか？
としらべたら以下のページがヒットした。

[同期プロトコルと非同期プロトコル - Blog by Sadayuki Furuhashi](http://frsyuki.hatenablog.com/entry/20081014/p3)


> ネットワークプロトコルは、
>  
> 1. リクエストした順番通りにレスポンスを返す（リクエストの順番とレスポンスの順番が同期している）
> 2. リクエストした順番通りにレスポンスが返ってくるとは限らない（リクエストの順番とレスポンスの順番が同期していない）
>
> の２種類に分けられる。
>  
> 前者を同期プロトコル、後者を非同期プロトコルと命名してみる。
> HTTP や XML-RPC は同期プロトコルであり、JSON-RPC は非同期プロトコルである。TCPも非同期プロトコルだと言える。非同期プロトコルの特徴として、パケットの中にシーケンス番号（かそれに類するもの）が含まれている。

JSON-RPC は例えばリスエストとレスポンスに シーケンスIDのフィールドを持つ。非同期ゆえ。

さて、ロボットのAPIだが、悩ましい。

[RPCとJSON-RPCAdd Star-廃墟](http://d.hatena.ne.jp/hagino_3000/20101209/1291831632)

## 他の WebAPI

物理デバイスをWeb経由で操作するAPIについて調査してみる。カメラばっかりなのは、私がカメラを趣味にしているから

### SONY Camera Remote API

[Camera Remote API beta SDK-はじめに | Sony Developer World](https://developer.sony.com/ja/camera-remote-api-beta-sdk-page/)

JSON-RPC。非常に綺麗な実装に見える。

OPC で非常に面妖なことになっているライビュービューだが、SONY Camrera Remote API ではかなりスマート。
startLiveview　でストリームを開始するURLを提供しているので、こういうテクニックは利用したい

```json:(こんなリザルトがかえる)
{
    "result": [
        "http://ip:port/liveview/liveviewstream"
    ],
    "id": 1
}
```

エンドポイントとしては &lt;ActionList_URL&gt;/camera、&lt;ActionList_URL&gt;/avContent 、 &lt;ActionList_URL&gt;/system がある。

イベントは getEvent を投げると、現在起きているイベントの一覧が帰ってくる、という理解でよいかな？
（引数 polling flag　を true にすると、timeout または change point detection の時に Callback される様子)

### RICHO THETA

- [Getting Started THETA DEVELOPERS](https://developers.theta360.com/ja/docs/v2.1/api_reference/getting_started.html)

このAPIバージョンは [Open Spherical Camera API](https://developers.google.com/streetview/open-spherical-camera/) をベースに拡張されている。

JSON-RPC 風の API だが、リクエスト・レスポンスにシーケンス番号を含まない。
ただ、SessionIDは持っていて、タイムアウト値を指定できるインターフェイスになっている。

### OPC Communication Protocol (OLYMPUS AIR)

[CAMERA KIT FOR DEVELOPERS | オリンパス OPC Hack and Make Project](https://opc.olympus-imaging.com/tools/sdk/)

けっこうきちゃない。組み込みやさんが付け焼き刃でつくったような印象を受ける。

1. コマンド通信 (TCP / HTTP)
2. ライブギュー画像転送 (UDP / RTP)
3. イベント通知 (TCP / 独自プロトコル) 



### はてなブックマークAtomAPI

[はてなブックマークAtomAPI](http://d.hatena.ne.jp/keyword/%A4%CF%A4%C6%A4%CA%A5%D6%A5%C3%A5%AF%A5%DE%A1%BC%A5%AFAtomAPI)

REST API。

## REST とは？

REST は、リソース毎にURIを切っておいて、HTTPのGET, POST, PUT, DELETEメソッドで操作を行なうような WebAPIのガイドラインだ。

例えば

| URI              |メソッド| 働き                         |
|------------------|--------|------------------------------|
| ./v1/entries     | GET    | エントリー一覧の取得         |
| ./v1/entries     | POST   | 新しいエントリーの作成       |
| ./v1/entries/:id | GET    | :id で示したエントリーの取得 |
| ./v1/entries/:id | PUT    | :id で示したエントリーの編集 |
| ./v1/entries/:id | DELETE | :id で示したエントリーの削除 |

というふうになる。((`v1` は APIバージョン ))

エラーコードには、HTTP状態コードを使う。また、理想としてはステートレスであることが要求される。

* [REST 入門(その8) REST でないもの](http://yohei-y.blogspot.jp/2005/05/rest-8-rest.html)
* [REST 入門(その5) 四つの動詞 -- GET, POST, PUT, DELETE](http://yohei-y.blogspot.jp/2005/04/rest-5-get-post-put-delete.html)

REST は非常に強い制約を持つおかげで統一的なアクセスが可能となる。

## JSON-RPC とは？

規格はこちら。

[JSON-RPC 2.0 Specification](http://www.jsonrpc.org/specification)

エンドポイントUILに対して、JSON で実行したいメソッドをなげ、JSONでレスポンスを受け取る


```json:リクエストボディの例 (Sony Camera Remote API/ deteteContent(V1.1)
{
  "id"    : 1
  "method": "deleteContent",
  "params": [
      {
          "uri" : [
              "image:content?contentId=XXXXXXXX",
              "video:content?contentId=XXXXXXXX"
          ]
      }
  ],
  "version": "1.1"
}
```

```json:レスポンスの例
{
    "id": 1,
    "result" : []
}
```

id を省略した場合、通知になる。また複数のAPIをシーケンシャルに叩く「バッチ」という規定もある。

* [なんとなくわかる JSONRPC 2.0 - Qiita](http://qiita.com/M_Nagata/items/4b0eede420cd5f3a5ae8)
* [CocoaでJSON-RPC - 廃墟](http://d.hatena.ne.jp/hagino_3000/20120313/1331635663)
* [JSON-RPC over HTTPなサーバを作る - Qiita](http://qiita.com/niku4i/items/bdef1e360729042673ad)



## どうする

デバイスを操るという視点では、RPC が良いと思う。一方で、ログを取得したり、スクリプトをアップロードしたりとはか REST と相性がいい。

現状WebAPIといえば猫も杓子も REST と言った風潮になっている。Hackableでやっていることが見えやすく、かつ動きが想像しやすいAPIだからと思う。

２つの技術が同等であれば、REST を選ぶべきと考えるが、
問題は、REST では、ロボットを動かしたりすることができない（REST的な概念ではない) というところだと思う。

なので、現時点での判断において考えるべきは

1. JSON-RPC で統一する
2. REST的な中で、できないものだけ JSON-RPC的、もしくは別の手立てで頑張る

のどちらにすべきかという問題と考える。

後者は以下のようなREST親和性の高いものに対しては、RESTを目指す (ただ、ログとかファイルでかいので、JSONじゃないコンテナになるかもね)

| Endpoint URL                     | METHOD                         |                                                     |
|----------------------------------|--------------------------------|-----------------------------------------------------|
| ./v1/scenarios                   | [GET/PUSH] /:id[GET/PUT/DELETE]|                                                     |
| ./v1/scenarios/:id/bind-devices  | [GET/PUSH]                     | ※個別デバイスにアクセス出来る必要はなさそう        |
| ./v1/logs/scenario               | [GET] /:id[GET/DELETE]         |                                                     |
| ./v1/logs/controller             | [GET] /:id[GET/DELETE]         |                                                     |
| ./v1/devices                     | [GET/PUSH] /:id[GET/PUT/DELETE]|                                                     |

また、ロボットの状態把握系に関しても、REST風は目指せると思う

| Endpoint URL                     | METHOD                         |                                                     |
|----------------------------------|--------------------------------|-----------------------------------------------------|
| ./v1/robot/joints                | [GET] :/id[GET]                |                                                     | 
| ./v1/robot/end-effector          | [GET] :/id[GET]                |                                                     | 
| ./v1/robot/errors                | [GET] :/id[GET]                |                                                     | 
| ./v1/robot/alerms                | [GET] :/id[GET]                |                                                     | 
| ./v1/controller/scenario-player  | [GET]                          |                                                     | 

一方で、ロボットを動かしたり、シナリオを再生したりは無理にREST風に落とす必要はないと思う。JSON-RPC がいい。
そういうの用のエンドポイントを用意しておくのが良いだろう。

* ./v1/robot/execute
* ./v1/controller/execute

たとえば、こんなのを送る

```
{
  "method": "move_end_effector_position",
  "params: [
      {
        "pos": {"x":0.0, "y":0.0, "z":0.0, "roll":0.0, "pitch":0.0, "yaw":0.0},
        "t":   0.0
      }
  ],
  "id": 1
}
```

WebSocket で繋げば、タイムアウトは現実的に考えないでもOKになるし、双方向通信もできるので操作系は別口、WebSocket、双方向で定義するのが吉かな。

リアルタイム通信まわりはとりあえず以下を読んでおこう。

* [リアルタイム通信で利用されるプロトコルと手法](http://tech.guitarrapc.com/entry/2015/08/17/044937#WebSocket)


