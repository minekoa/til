# npm の パッケージを更新する

基本的には package.json を編集して `npm install` コマンドを実行すればいい

[npmのpackage.jsonと依存関係を理解しよう！ -bagelee (ベーグリー)](https://bagelee.com/programming/npm-package-json-dependencies/)

頭の `^` とか `~` とかの記法

> ```
>  4.17.10: 完全一致 (version = 4.17.10)
> ^4.17.10: メジャーバージョンが一致 (4.17.10 <= version < 5.0.0)
> ~4.17.10: マイナーバージョンが一致 (4.17.10 <= version < 4.18.0)
> ```


* [npmのpackage.jsonを最新のバージョンに更新する | omachizura.com](https://omachizura.com/2016/02/npm-package-new.html)
* [package.jsonに書かれた依存ライブラリを一括でupdateする | Qiita](https://qiita.com/masterkey1009/items/b0020b61e83f21c6d4d0)

`npm-check-apdates` を使うと
かってに package.json を更新してくれる
