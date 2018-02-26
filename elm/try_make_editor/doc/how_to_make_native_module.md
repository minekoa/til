# Elm Nativeモジュールの作り方

## オーバービュー

Elm0.17以降では、モジュールを拡張するためにあるルールにそって命名されたjavascriptの変数にモジュールをセットすることでElmからアクセス出来るようになる。

変数は、`elm-package.json`の `repository` に基づいて決められる。

```json:elm-package.json
    "repository": "https://github.com/minekoa/project.git",
```

```javascript:Mice.js
var _minekoa$project$Native_Mice
```

`_`で始まりユーザ名・プロジェクト名を `/` の代わりに `$`でセパレートして書き、最後に `Native_<ファイルのベース名>` とする。

また、`elm-package.json` に `"native-modules": true` の一文を足す必要がある。これは
Nativeが含まれるモジュールは型的に安全とは言えないので、それをモジュール自身に表明させたいためとのこと。

JavaScriptの置き場所は、Native/ の下でなくてはいけない。


## 1. elm-package.json に Nativeを使うことを書く

`"native-modules": true` を追加します

```json
{
        ・
        ・
    "repository": "https://github.com/minekoa/project.git",
        ・
    "native-modules": true,
        ・
}
```

## 2. JavaScriptを書く

`Native/ファイル名.js` というファイルを作り、そこに記述する。


```javascript
var _minekoa$project$Native_Mice = function() {

  function doFocus(_id) { 
	  var element = document.getElementById(_id); 
      if (element == null) {
          return false;
      }

	  element.focus();
      return true;
  } 

  return {
    doFocus: doFocus
  }
}();
```

普通のよくあるJavaScriptのモジュール（export するものをオブジェクトで返す即時実行関数）で書き、先ほどのルールで作った変数に入れる。


## 2. Elmから呼ぶ(インターフェイス関数を書く)

```elm
import Native.Mice

doFocus : String -> Bool
doFocus id = Native.Mice.doFocus id
```

普通に呼べます。


## 参考

* [Elm 0.17~0.18版 NativeModuleの書き方](https://qiita.com/k-motoyan/items/24f8b5f27ab828efb024)
** `elm-package.json` に追記が必要な話が抜けていたのと、関数を Fn でラップする話がうまく行かなかった(F1だから？）
* [Elmのネイティブモジュールが見つからないと言われる|stackoverflow](https://ja.stackoverflow.com/questions/20947/elm%E3%81%AE%E3%83%8D%E3%82%A4%E3%83%86%E3%82%A3%E3%83%96%E3%83%A2%E3%82%B8%E3%83%A5%E3%83%BC%E3%83%AB%E3%81%8C%E8%A6%8B%E3%81%A4%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A8%E8%A8%80%E3%82%8F%E3%82%8C%E3%82%8B)
** 上記 `elm-package.json`追記が抜けているの話
