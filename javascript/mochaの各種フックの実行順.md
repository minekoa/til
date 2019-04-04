# mocha の各種フックの実行順序

* [mocha における describe it before beforeEach after afterEach の実行順序 | Qiita](https://qiita.com/sta/items/2c89af1e05202fb76b96)
* [mochaでテストを書くときに気をつけたいこと with Sinon.JS | Qiita](https://qiita.com/putan/items/2feac143d4a28caf884d)
* [テストフレームワーク mocha | hokaccha memo](https://hokaccha.hatenablog.com/entry/20111202/1322840375)

結論からいうと

* before, after を複数登録すると登録順に実行される
* 非同期はちゃんと待つ
* 複数登録した after の一つが例外を発すると、以後のafterは実施されない


## 実験

```javascript
function sleep(msec) {
    return new Promise(resolve => setTimeout(resolve, msec))
}

describe('should reject the request with 400', () => {
    console.log("## TEST");

    before("A", async () => {
        console.log("# BEFORE A");
        await sleep(1000);
        console.log("# BEFORE A2!");
    });

    console.log("## TEST 2");

    after("A", async () => {
        console.log("# AFTER A1!");
        await sleep(1800);
        console.log("# AFTER A2!");
    });

    console.log("## TEST3");

    after("B", async () => {
        console.log("# AFTER B1!");
        await sleep(1000);
        console.log("# AFTER B2!");
    });

    console.log("## TEST4");

    it( 'because this title is longer than 256 chars', async () => {
        console.log("# it XA1");
        await sleep(1000);
        console.log("# it XA2");
    });
    it( 'because this body is longer than 30000 chars', async () => {
        console.log("# it XB1");
        await sleep(1000);
        console.log("# it XB2");
    });
    it( 'because this title already exists', async () => {
        console.log("# it XC1");
        await sleep(1000);
        console.log("# it XC2");
    });
});
```

```console
===> Running test cases
## TEST
## TEST 2
## TEST3
## TEST4

      should reject the request with 400
# BEFORE A
# BEFORE A2!
# it XA1
# it XA2
        ✓ because this title is longer than 256 chars (129ms)
# it XB1
# it XB2
        ✓ because this body is longer than 30000 chars (136ms)
# it XC1
# it XC2
        ✓ because this title already exists (110ms)
# AFTER A1!
# AFTER A2!
# AFTER B1!
# AFTER B2!
```

## 解説

describe の中は普通に評価される。
（複数のテストがある場合は全てのテストのdescribeがまず評価される）

その際に it とか before とかが評価されると
そのλがどっかに保存されるん。

あとdescribe の変数とを見るようになるから
レキシカルクロージャになってる？

動的スコープでゴニョゴニョしてる可能性も除外できないというか、
mocha の機能とか(lambdaのなかで条件によってskipとか）使おうとすると
アローファンクションじゃだめで、ダイナミックスコープ的な
伝統的 function な lambda を使わないといけない。
