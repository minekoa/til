# BDD のプラクティス（ツールを例示としてみながら理解しよう）

BDD は 要求分析の成果物としてのテスト を作ることに見える。

TDDまでと違うのは、このテストをみるのが、プログラマ、QAだけでなく
顧客（要求の出処）やステークホルダも見るということだ。

なので、プログラミング言語をしらなくてもテストが読めるように…というのが
BDDのテストフレームワーク (RSpecやchaiのマッチャー) のサポートすべき機能になる。

（ここが TDDとの違いになる）

各サイトの記事をつまみ食いしながら、その感覚をつかむ

## 振る舞い駆動開発(BDD)とは

[振る舞い駆動開発(BDD)とは | テスト駆動開発／振る舞い駆動開発を始めるための基礎知識 (3/3)](https://www.atmarkit.co.jp/ait/articles/1403/05/news035_3.html#091)

> BDDでは、テストの記述において「Tests as Documentation」
> 「Specification by Example」という2つの考え方が重視されます。

ふむん。

> 「Tests as Documentation」はテストをドキュメントとして扱うアプローチ
> です。テストの可読性を上げ、テスト対象の振る舞いを分かりやすく記述す
> ることにより、テストをテスト対象の詳細仕様書のように扱えるようにしま
> す。これはテスト対象の保守に活用されます。

BDD、真面目にやるなら ADTの仕様記述と1:1になるようなテストを書くことになる。
すたっく、すたっくかぁ…。メイヤー先生、スタック定義しますよ！

> 「Specification by Example」は、例示による要求や仕様の記述を活用する
> ための包括的な考え方です。例示を使って要求獲得や要求仕様化を行うこと
> で、ユーザーとのコラボレーションや変化への対応、ユーザー要求と設計と
> の関連付けを促進することを目的としています。

## ユーザーストーリ

BDDはユーザストーリからテストを起こすそうだ。

https://agile.esm.co.jp/services/agile/process.html

> ユーザーストーリーはユーザーが実現したいことやユーザーにとって価値があることを簡潔にまとめた文章です。

[簡単！楽しい！5分でわかるユーザーストーリーマッピング(User Story Mapping)|Qiita](https://qiita.com/Koki_jp/items/6aebc73bedd0a932dcb8)

> 電子メール管理システムの場合
>
> 1.ユーザは電子メールを検索できる。
> 2.ユーザは電子メールをファイリングできる。
> 3.ユーザは電子メールをキーワードで検索できる。
> 4.ユーザは電子メールを移動できる。
> 5.ユーザはサブフォルダを作ることが出来る。なぜならそこに電子メールを移動させたいからだ。

こんな感じか。

身近な実例にに落としてみよう

* 《特権ユーザー》は《サンプルデータ》を生成できる
* 《特権ユーザー》は生成した《サンプルデータ》を○○で検索できる
* 《特権ユーザー》は生成した《サンプルデータ》の一覧を閲覧できる
* 《特権ユーザー》は生成した《サンプルデータ》を○○で削除できる
* 生成した《サンプルデータ》は《特権ユーザー》以外から守られる

て感じかな。これをどうテストに落とせばいいんだ？

## BDDの話をしよう

[BDDの話をしよう | やさしいデスマーチ](http://d.hatena.ne.jp/shuji_w6e/20101226/1293290205)

tumbler-glass というツールの紹介を通して、BDDをステップ・バイ

> では、本題です。Google Codeにtumbler-glassというライブラリがあったの
> で試してみました。「coding-by-example library」と説明があるように、
> サンプル（例）からコードを書いていくためのライブラリで、BDD（ビヘイ
> ビア駆動開発）を促進し、「考える」事をサポートするらしいです

> ## シナリオを書く
>
> シナリオは最初からJavaのコードで書くか、テキストで起こして変換します。
> テキストから変換する方がプログラマでないマネージャなどにウケがいいよ
> ね、とか書かれてます。

で、ここから変換された（あるいは手で書いて良い）テストコードの雛形がこれ。

> ```
> @RunWith(TumblerRunner.class)
> @Story("el shaddai")
> public class ElShaddaiScenarios {
>
>     @Scenario(pending = true)
>     public void shouldBeAttackedByEnemiesAndDie() {
>         Given("イーノック and ノーマル装備");
>
>         When("大丈夫だ、問題ない");
>
>         Then("死ぬ");
>
>     }
>
>     @Scenario(pending = true)
>     public void shouldDestroyAllEnemies() {
>         Given("イーノック and ノーマル装備");
>
>         When("一番いいのを頼む");
>
>         Then("勝利する");
>
>     }}
> ```

で、ここからテストコードを追記していく

> ```
>  @Scenario
>     public void shouldBeAttackedByEnemiesAndDie() {
>         Given("イーノック and ノーマル装備");
>         Enoch enoch = new Enoch();
>         enoch.equiq(new NormalArmor());
>         When("大丈夫だ、問題ない");
>         // do nothing
>         Then("死ぬ");
>         assertThat(enoch.battle(), is(Status.DEAD));
>     }
>
>     @Scenario
>     public void shouldDestroyAllEnemies() {
>         Given("イーノック and ノーマル装備");
>         Enoch enoch = new Enoch();
>         enoch.equiq(new NormalArmor());
>         When("一番いいのを頼む");
>         enoch.equiq(new AngelArmor());
>         Then("勝利する");
>         assertThat(enoch.battle(), is(Status.ALIVE));
>     }
> ```

ユースケースシナリオのように読めるテストが出来上がる仕組み。

> tumblerの目玉として、綺麗なレポートを作成することができます

これいいな。
画像引用はめんどいなので、写す。

こんな感じらしい

| el shaddai should:             | Description                       | Run status |
|--------------------------------|-----------------------------------|------------|
| be attacked by enemies and die | Given イーノック and ノーマル装備 |            |
|                                | When 大丈夫だ、問題ない           |            |
|                                | Then 死ぬ                         |            |
|--------------------------------|-----------------------------------|------------|
| destroy all enemies            | Given イーノック and ノーマル装備 |            |
|                                | When 一番いいのを頼む             |            |
|                                | Then 勝利する                     |            |

KML前のExcel表現仕様に似てる。

* eventがない (コード的には enoch.battle() )
* Given 事前の状態
* When イベントのガード条件あるいは事前の条件
* Then 事後の状態

でいいのかなぁ？
じつは When を最重要視してる？


## chaiをやめてpower-assertを使うことにした

[chaiをやめてpower-assertを使うことにした](https://web-salad.hateblo.jp/entry/2016/01/26/083000)

本旨は、it の引数と関数のボディほとんどいっしょじゃね？
これ Mocha が全然 BDD 考えていないからだよね. chai 意味ないじゃん。。。的な話。

（ここから、BDDのテスティングフレームワークとそれを使ったテストコードのあるべき姿が見える？）

> ```
> const chai = require("chai");
> const expect = chai.expect;
>
> describe("Array.from()", () => {
>   let subject;
>
>   context('when "foo" is passed', () => {
>     beforeEach(() => {
>       subject = Array.from("foo");
>     });
>
>     it('should include "foo"', () => {
>       expect(subject).to.include("foo");
>     });
>
>     it('should include "f"', () => {
>       expect(subject).to.include("f");
>     });
>   });
> });
> ```

shouldをつかうと更にツラミ倍増かな？

```
it ('should include "foo"', () => {
    subject.should.include("foo");
});
```

RSpec の場合は

> 例えば、こんな感じに書くと
>
> ```
> RSpec.describe "Kernel#Array" do
>   subject { Array(arg) }
>
>   context 'when "foo" is passed' do
>     let(:arg) { "foo" }
>
>     it { is_expected.to include "foo" }
>     it { is_expected.to include "f" }
>   end
> end
> ```

> テスト失敗時にexpected ["foo"] to include "f"というメッセージが得ら
> れるのはもちろんのこと、成功時にもshould include "foo"というメッセー
> ジが自動で生成されていることがわかります。また、it { is_expected.to
> include "foo" }という一行はもはや英語として解釈することができますね＼
> (^o^)／
