# デフォルトのキーボード・ショートカットを殺す

## 帯に短し..

いちおう、`onWithOption` を使えばキーイベントは殺すことができる。

オプションは、

```elm
type alias Options = 
    { stopPropagation : Bool
    , preventDefault : Bool
    }
```

* stopPropagationがtrueの場合、イベントはDOMを通過しなくなり、他のイベントリスナーはトリガーされません。 
* preventDefaultがtrueの場合、イベントに関連する組み込みのブラウザー動作は防止されます。

問題にしていた挙動を封殺するには、preventDefault を True にすればいい…

```elm
onKeyDown : (KeyboardEvent -> msg) -> Attribute msg
onKeyDown tagger =
    onWithOption "keydown" (Options False True) (Json.map tagger decodeKeyboardEvent)
```

…のですが、
結果、キーインプットも動かなくなっちゃいます。キーボード・ショートカットのときだけ殺す必要があるのです。

## やったこと

Nativeモジュールで以下のようなJavaScriptを作り、そちらを登録します

```javascript
  function setPreventDefaultKeyShortcut (_id) {
	  const element = document.getElementById(_id); 
      if (element == null) {
          return false;
      }

	  element.addEventListener( "keydown", e => {
          if (e.altKey || e.ctrlKey) {
              e.preventDefault();
          }
      });
      return true;
  }
```

ちな、elm側からの呼び出しは domのレンダリングが終わった後である必要があるため、init のモデル構築のさなかではやっちゃダメ。


## もっと困った話

ちなみに、Chrome側の一部のキーボード・ショートカットはそれでも殺せません。 Ctr-N とか。

そもそも keydown イベントが着ていないようなので打つ手なしと思われます（多分。要調査）

## 参考

* [Html.Events - elm-html 4.0.1](http://package.elm-lang.org/packages/evancz/elm-html/4.0.1/Html-Events#onWithOptions)
* [event.preventDefault | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/Event/preventDefault)
* [KeyboardEvent | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/KeyboardEvent)

* [JavaScript でイベント伝播の止め方いろいろ](http://d.hatena.ne.jp/modified/20120615/1339731561)
