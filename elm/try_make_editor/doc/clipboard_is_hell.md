# クリップボードは地獄や

## copy

`document.execCommand('copy')` でクリップボードにデータをコピーします。
コピー元のデータはDOM上で「選択されているもの」になるので、選択操作もやってあげます。

（spanとかでもできるけれど、input系のほうが格段にらく）

```javascript
  function copyToClipboard (_id, txt) {
	  const element = document.getElementById(_id); /*textarea であることを期待している */
      const range = document.createRange();

      element.value = txt;
      element.select();
      return document.execCommand('copy');
  };
```

コピー専用テキストエリアを使って、クリップボードにコピーする。

このとき、フォーカスがこのテキストエリアに写ってしまうので、
これの処理をElmのTaskとして実装して、Elm 側の updateで終了をフックし、
フォーカスをもとのInputに戻してあげる処理を書けば良い

## paste

paste はセキュリティのため、使いにくい

* 1. paste は、ブラウザのペースト挙動でしか発火することができない
   * Ctrl-y
   * 右クリックから貼り付け
* 2. paste イベント中でないと、クリップボードのデータを読み込むことはできない
   * （コピーやカットでクリップボードのデータを読み込むことはできない）
   * （ペーストの直前にクリップボードのデータを書き換えることはできない）

という大きな制約がある。

これにより、残念ながら
任意のトリガー（たとえば、C-y とか 「ペーストボタン」なるものを置くとか）はできない（っぽい）
そこは諦めよう。



### 作戦

Paste実現のため、以下の作戦で実装した

* Ctrl-v は Elm側でイベントを起こすのではなく、ブラウザの paste イベントが ピタゴラして実施されるようにする
    * ブラウザのペースト
    * input用 textarea に paste のイベントハンドラを設置
       * paste 専用テキストエリアに貼り付けられた文字をコピー
       * inputイベント@paste_areaを人為的に発生
    * elm側で paste_area のinputイベントをフックし、paste処理を実施

という流れにした

やることは

* keydown で preventDefault をしているところに Ctrl-v だけ穴を開ける
* peste イベントハンドラを記載
   * ここでpreventDefault する
   * ペースト専用エリアにクリップボードの内容をコピー
   * inputイベントを発火
* elm 側で paste_area の onInput s に紐づけて挿入処理を実施


### 人為的にイベントを起こす

```javasctipt
var event = new Event('input', {
    'bubbles': true,
    'cancelable': true
});

element.dispatchEvent(event);
```

## 新たなる問題

(http://claytonflesher.github.io/2016/11/01/copy-to-clipboard.html)
> ## なぜそれが動作するのですか
> 
> これは実際にElmの興味深い問題です。 ELmの素晴らしい点の1つは、 副作用がないということです。 
> これは、コードの読みやすさ、Elmの信じられないほどのエラーメッセージ、コードがコンパイルされていれば大いに役立つという知識のような大きなメリットをもたらします。
> 
> 残念なことに、クリップボードへのコピーは、定義上、副作用です。 
> つまり、Elmの内部にないJavaScriptを利用したり、Elmに副作用をもたらすElmネイティブライブラリを作成したりすることなく、実際にはElmの内部から行うことはできません。 
>  私たちは間違いなく後者をしたくないので、前者を解決しなければなりません。
> 
> Clipboard.jsはこれを行いますが、一見するとどのようになっているのかははっきりしません。
> 
> Clipboard.jsを使用せずにElm自身からクリップボードへのコピーを実装しようとした人は、Firefox上で動作しないという問題に遭遇している可能性があります。 
> もともと私たちはElmがJavaScriptの世界にメッセージを送り、実際にそのコピーを行ったいくつかのJSコードを持っているので、これをすべて実行しようとしました。 
> これはGoogle Chromeでもうまくいきましたが、Firefoxはコンソールにエラーを投げて、コピーを実行しているものがユーザーによって直接行動を起こさなければならないことを教えてくれました。 
> クリックでメッセージを送信するインダイレクションのレイヤーを作成することはできませんでした。そのメッセージは実際にコピーを作成したJSに送られました。 それはセキュリティの偽のパスです。
> 
> ですから、Clipboard.jsはこの問題をどのように解決したのでしょうか？ `<body>` イベントリスナーを関連付けます。 
> イベントが発生するたびに、リスナーはそれが視聴している種類のものかどうかを確認します。 そうであれば、コールを傍受してアクション自体を実行します。
> それ以外の場合は、コールがツリーの下に移動します。
> 
> イベントリスナーが`body`上にあるため、Elmが`div`再レンダリングしても、リスナーが存続します。

調べてみたら、clipboard.js がうまいことやってるんじゃなくって、elm から clipboard.js を使うと、bodyのonclickに addEventListener されて、その中で処理されるようになるので、
Elm側でもいい感じに動くという話だったので、今回の参考にはならない...orz


## 参考

* document.execCommand('copy')
    * [Interact with the clipboard | MDN web docs](https://developer.mozilla.org/en-US/Add-ons/WebExtensions/Interact_with_the_clipboard)
    * [Selection.addRange() | MDN web docs](https://developer.mozilla.org/en-US/docs/Web/API/Selection/addRange)
    * [ライブラリを使わずJSでコピペをカスタマイズ](https://qiita.com/kwst/items/8d9cd40e181761085325)
    * [document.execCommand('copy')してみる | Qiita](https://qiita.com/keiskimu/items/a8128e14ef8b60c681b2)
    * [【javascript】クリップボードにコピーさせる方法 |ysk log](http://ysklog.net/javascript/2157.html)

* Clipboard API
    * [Clipboard API について](https://hakuhin.jp/js/clipboard.html#CLIPBOARD_EVENT)
    * [ClipboardEvent | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/ClipboardEvent)
    * [copy | MDN web docs](https://developer.mozilla.org/ja/docs/Web/Reference/Events/copy)
    * [ライブラリを使わずJSでコピペをカスタマイズ | Qiita](https://qiita.com/kwst/items/8d9cd40e181761085325)

* DataTransfer
    * [DataTransfer クラスについて](https://hakuhin.jp/js/data_transfer.html)
    * [DataTransfer.getData() | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/DataTransfer/getData)


* dispatchEvent
    * [How do I programmatically trigger an “input” event without jQuery?](https://stackoverflow.com/questions/35659430/how-do-i-programmatically-trigger-an-input-event-without-jquery)
    * [JavaScript Tips – dispatchEvent を使いこなそう!!](http://phiary.me/javascript-tips-dispatchevent/)
