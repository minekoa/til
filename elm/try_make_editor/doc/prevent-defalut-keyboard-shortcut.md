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

* [Chrome のキーボード ショートカット | Google Chrome ヘルプ](https://support.google.com/chrome/answer/157179?hl=ja)

一応調べてみた (バージョン 53.0.2785.143 Built on Ubuntu , running on Ubuntu 16.04 (64-bit))

(タブとウインドウのショートカット)

|ショートカット   |抑止できる？|備考                                            |
|-----------------|------------|------------------------------------------------|
| Ctrl-n          | ☓         | 新しいウインドウを開く                         |
| Ctrl-Shift-n    | ☓         | 新しいウィンドウをシークレット モードで開く    |
| Ctrl-t          | ☓         | 新しいタブを開いてそのタブに移動する           |
| Ctrl-Shift-t    | ☓         | 最後に閉じたタブを開いてそのタブに移動する     |
| Ctrl-Tab        | ☓         | 開いている次のタブに移動する                   |
| Ctrl-PgDn       | ☓         | 開いている次のタブに移動する                   |
| Ctrl-Shift-Tab  | ☓         | 開いている前のタブに移動する                   |
| Ctrl-PgUp       | ☓         | 開いている前のタブに移動する                   |
| Ctrl-1 〜 Ctrl-8| ○         | 特定のタブに移動する                           |
| Ctrl-9          | ○         | 最後のタブに移動する                           |
| Alt-Home        |            | ホームページを現在のタブで開く                 |
| Alt-←          | ○         | 現在のタブの閲覧履歴の中で前にあるページを開く |
| Alt-→          | ○         | 現在のタブの閲覧履歴の中で次にあるページを開く |
| Ctrl-w          | ☓         | 現在のタブを閉じる                             |
| Ctrl-F4         | ☓         | 現在のタブを閉じる                             |
| Ctrl-Shift-w    |            | 開いているすべてのタブとブラウザを閉じる       |
| Alt-Space-n     |            | 現在のウィンドウを最小化する                   |
| Alt-Space-x     |            | 現在のウィンドウを最大化する                   |
| Alt-F4          |            | 現在のウィンドウを閉じる                       |
| Ctrl-Shift-q    |            | Google Chrome を終了する                       |


(Google Chrome 機能のショートカット)

|ショートカット  |抑止できる？|備考                                                 |
|----------------|------------|-----------------------------------------------------|
| Alt-f          |            | Chrome メニューを開く                               |
| Alt-e          |            | Chrome メニューを開く                               |
| F10            |            | Chrome メニューを開く                               |
| Ctrl-Shift-b   | ○         | ブックマーク バーの表示と非表示を切り替える         |
| Ctrl-Shift-o   |            | ブックマーク マネージャを開く                       |
| Ctrl-h         | ○         | 履歴ページを新しいタブで開く                        |
| Ctrl-j         | ○         | ダウンロード ページを新しいタブで開く               |
| Shift-Esc      | ☓         | Chrome タスク マネージャを開く                      |
| Shift-Alt-t    | ○         | Chrome ツールバーの最初の項目にフォーカスを移動する |
| F6             | -          | アドレスバー、ブックマーク バー（表示されている場合）、ページ コンテンツの間でフォーカスを移動する（順方向）|
| Shift-F6       | -          | アドレスバー、ブックマーク バー（表示されている場合）、ページ コンテンツの間でフォーカスを移動する（逆方向）|
| Ctrl-f         | ○         | 現在のページ内を検索する検索バーを開く              |
| F3             | -          | 現在のページ内を検索する検索バーを開く              |
| Ctrl-g         | ○ (多分)  | 検索バーでの検索内容に一致する次の部分に移動する    |
| Ctrl-Shift-g   |            | 検索バーでの検索内容に一致する前の部分に移動する    |
| Ctrl-Shift-j   | ○         | デベロッパー ツールを開く                           |
| F12            | -          | デベロッパー ツールを開く                           |
| Ctrl-Shift-Del | ○         | [閲覧履歴データを消去する] オプションを開く         |
| F1             | -          | Chrome ヘルプセンターを新しいタブで開く             |
| Ctrl-Shit-m    | ○         | 別のユーザーでログインする、またはゲストとしてブラウジングする |
| Alt-Shift-i    | ?          | フィードバック フォームを開く |



(ウェブページのショートカット)

|ショートカット       |抑止できる？|備考                                                         |
|---------------------|------------|-------------------------------------------------------------|
| Ctrl-p              | ○         | 現在のページを印刷するオプションを開く                      |
| Ctrl-s              | ○         | 現在のページを保存するオプションを開く                      |
| F5                  | -          | 現在のページを再読み込みする                                |
| Ctrl-r              | ○         | 現在のページを再読み込みする                                |
| Shift-F5            | -          | キャッシュ コンテンツを無視して現在のページを再読み込みする |
| Ctrl-Shift-r        |            | キャッシュ コンテンツを無視して現在のページを再読み込みする |
| Esc                 | -          | ページの読み込みを停止する                                  |
| Tab                 | -          | クリック可能な項目間を移動する（順方向）                    |
| Shift-Tab           | -          | クリック可能な項目間を移動する（逆方向）                    |
| Ctrl-o-ファイル選択 |            | パソコンのファイルを Chrome で開く                          |
| Ctrl-u              |            | 現在のページの HTML ソースコードを表示する（編集不可）      |
| Ctrl-d              |            | 現在のウェブページをブックマークとして保存する              |
| Ctrl-Shift-d        |            | 開いているすべてのタブをブックマークとして新しいフォルダに保存する |
| F11                 |            | 全画面表示モードをオンまたはオフにする                      |
| Ctrl-(+)            | ○         | ページ上のすべての要素を拡大する                            |
| Ctrl-(-)            | ○         | ページ上のすべての要素を縮小する                            |
| Ctrl-0              | ○         | ページ上のすべての要素をデフォルトのサイズに戻す            |
| Space               |            | ウェブページを 1 画面ずつ下にスクロールする                 |
| PgDn                |            | ウェブページを 1 画面ずつ下にスクロールする                 |
| Shift-Space         |            | ウェブページを 1 画面ずつ上にスクロールする                 |
| PgUp                |            | ウェブページを 1 画面ずつ上にスクロールする                 |
| Home                |            | ページの先頭に移動する                                      |
| End                 |            | ページの最後に移動する                                      |
| Shift-Wheel         |            | ページを水平方向にスクロールする                            |
| Ctrl-←             |            | テキスト欄内で前にある単語の先頭にカーソルを移動する        |
| Ctrl-→             |            | テキスト欄内で次にある単語の後ろにカーソルを移動する        |
| Ctrl-Bs             |            | テキスト欄内で前にある単語を削除する                        |
| Alt-n               |            | 通知にフォーカスを移動する                                  |
| Alt-Shift-a         |            | 通知内で許可する                                            |
| Alt-Shift-d         |            | 通知内で拒否する                                            |
| Alt-Home            | ○         | ホームページを現在のタブで開く                              |



## 参考

* [Html.Events - elm-html 4.0.1](http://package.elm-lang.org/packages/evancz/elm-html/4.0.1/Html-Events#onWithOptions)
* [event.preventDefault | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/Event/preventDefault)
* [KeyboardEvent | MDN web docs](https://developer.mozilla.org/ja/docs/Web/API/KeyboardEvent)

* [JavaScript でイベント伝播の止め方いろいろ](http://d.hatena.ne.jp/modified/20120615/1339731561)
