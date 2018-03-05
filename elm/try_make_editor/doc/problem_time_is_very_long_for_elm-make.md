# elm-make がとてもとても長い問題

範囲選択を実装したところ、いきなり elm-make が 10分以上かかるようになった。
なじぇ。

この問題について twitter でつぶやいたところ、

> この人生は刺繍をするには短すぎる @miyamo_madoka
> パターンマッチを(x, y, z,w)みたいなことしました？

> みねこあ @minekonya
> 3月2日
> 今回の修正差分には確かなかったとおもいますが、指摘のスタイルが結構ふくまれてます…
> ヤバいのですか？(^^;

> この人生は刺繍をするには短すぎる @miyamo_madoka
> 4,5個くらいからコンパイルが時間かかるようになるみたいです
> 確か既知で0.19修正入るって言ってたと思います
> 
> 今回差分に含まれてなかったら違うかもしれませんが・・・
> 原理も知らないのでしかとは言えないです


との指摘が。実は diff をみると確かに以下のようなパターンマッチ

```elm
keymapper : (Bool, Bool, Bool, Int) -> (Model -> Model)
keymapper (ctrl, alt, shift, keycode) =
   case (ctrl, alt, shift, keycode) of 
        (False, False, False,  37) -> moveBackward -- '←'
        (False, False, False,  38) -> movePrevios  -- '↑'
        (False, False, False,  39) -> moveForward  -- '→'
        (False, False, False,  40) -> moveNext     -- '↓'
        (False, False, True ,  37) -> selectBackward -- '←'
        (False, False, True ,  38) -> selectPrevios  -- '↑'
        (False, False, True ,  39) -> selectForward  -- '→'
        (False, False, True ,  40) -> selectNext     -- '↓'
        (False, False, _,       8) -> (\m -> backspace m (Buffer.nowCursorPos m.buffer)) -- BS
        (False, False, False,  46) -> (\m -> delete m (Buffer.nowCursorPos m.buffer))    -- DEL

        (True,  False, False,  90) -> undo         -- 'C-z

         -- emacs like bind
        (True,  False, False,  70) -> moveForward  --  'C-f'
        (True,  False, False,  66) -> moveBackward --  'C-b'
        (True,  False, False,  78) -> moveNext     --  'C-n'
        (True,  False, False,  80) -> movePrevios  --  'C-p'
        (True,  False, False,  72) -> (\m -> backspace m (Buffer.nowCursorPos m.buffer))   -- 'C-h'
        (True,  False, False,  77) -> (\m -> insert m (Buffer.nowCursorPos m.buffer) "\n") -- 'C-m'

         _                          -> (\m -> m)
```

に対して、新たなパターン（範囲選択の shift + →群) を追加していたのでした。

パターンを削除するとビルド時間が元の短さに。これだぁ〜。

## 付け焼き刃 -> ダメでした

タプルがだめなら値コンストラクタがあるじゃない、ということでコードを以下のように修正

```elm
type KeyCombination = KeyCombination Bool Bool Bool Int

keymapper : (Bool, Bool, Bool, Int) -> (Model -> Model)
keymapper (ctrl, alt, shift, keycode) =
    case KeyCombination ctrl alt shift keycode of
        KeyCombination False False False  37 -> moveBackward -- '←'
        KeyCombination False False False  38 -> movePrevios  -- '↑'
        KeyCombination False False False  39 -> moveForward  -- '→'
        KeyCombination False False False  40 -> moveNext     -- '↓'
        KeyCombination False False True   37 -> selectBackward -- '←'
```

結果、変わりませんでした...orz

## なにが起きているの？

* [Bad worst-case performance for pattern match compiler | Issue #1362 | elm-lang/elm-compiler](https://github.com/elm-lang/elm-compiler/issues/1362)
* [When Do Match-Compilation Heurestics Matter?](https://pdfs.semanticscholar.org/b8d1/e3b73db3e6549334cc7c20da060516c3188a.pdf) (PDF)

> 最近の静的型付けされた関数型言語は、パターンマッチングによって関数を定義します。 パターンマッチングは、
> あるパターンに対してあるパターンに対する値の順次チェックの観点から定義されますが、
> 実際の実装ではパターンを多数のパターンに対して同時にテストできるオートマトンに変換します。 
> 決定木は一般的なオートマトンです。
>
> 意思決定ツリーを使用するコストは、そのサイズと形状に関連しています。 最小コストの決定木を生成するために保証される唯一の方法は、
> 指数関数的なマッチコンパイル時間を必要とするため、多くのヒューリスティックスが文献で提案されているか、または実際のコンパイラで使用されている。
> 本稿では、New JerseyコンパイラのStandard MLを使用して、そのような発見的手法の実験的評価を提示する。 


パターンマッチオートマトンの生成にすごく時間がかかっているので、これは タプルだからという問題ではない。



