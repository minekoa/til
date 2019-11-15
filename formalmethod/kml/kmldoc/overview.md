# オーバービュー

小さな実例を示す

components.kml

```
type Button = { label : String
              , enabled : Bool
              };

type Input = { text : String
             , enabled : Bool
             }

type Label = { text : String
             , visible : Bool
             }
```

events.kml

```
event
  = entry                        @(- ページ遷移 -)
  | piyoInput_Inputed of String  @(- ぴよインプット入力 -)
  | doButton_clicked             @(- 実行ボタンクリック -)
  | clearButton_clicked          @(- 削除ボタンクリック -)
  | returnButton_clicked         @(- 戻るボタンクリック -)
  | exitButton_clicked           @(- アプリ終了ボタンクリック -)
```

SimplePage.kml

```
state SimplePage {
    var piyoInput : Input  = Input "" True;
    var doButton : Button = Button "実行する" False;
    var clearButton : Button = Button "クリア" True;

    invariant {
       hogeButton.enabled = (piyoInput.text != "")
    } @ {-
       hogeボタンは、piyoインプットが空 でないときに有効
    -}

    transition piyoInput_Inputed str
          when str != ""
           @[- 入力文字列が空でないとき -]
    --> {
        post {
            target piyoInput, doButton;
            piyoInput' = { piyoInput | text <- str };
            hogeButton' = { hogeButton | enabled <- True };
        } @ {-
            piyoインプットの入力値を更新する
            doボタンが有効である
        -}
    }

    transition piyoInput_Inputed str
          when str != ""
           @[- 入力文字列が空のとき -]
    --> {
        post {
            target piyoInput, doButton;
            piyoInput' = { piyoInput | text <- "" };
            hogeButton' = { hogeButton | enabled <- False };
        } @ {-
            piyoインプットの入力値を更新する
            doボタンが無効である
        -}
    }

    transition clearButton_clicked
    --> {
        post {
            target piyoInput, doButton;
            piyoInput' = { piyoInput | text <- "" };
        } @ {-
            piyoインプットが空である
            doボタンが無効である
        -}
    }

    transition doButton_clicked
    --> {
        post {
            target state;
            state' = SimpleMsgPage piyoInput.text;
        } @ {-
            piyoインプットが空である
            doボタンが無効である
        -}
    }
}
```

SimpleMsgPage.kml

```
state SimpleMsgPage (message : String) {
    var msgLabel : Label;
    var returnButton : Button = Button "戻る" True;
    var exitButton  : Button = Button "アプリケーションの終了" True;

    transition entry
    --> {
        post {
            target msgLabel;
            msgLabel' = Label message True;
        }@{-
            引数で渡されたメッセージがラベルに表示されている
        -}
    } @ {- ページに遷移したときのアクション -}

    transition returnButton_clicked
    --> {
        post {
             target state;
             state' = SimplePage;
        } @ {-
           SimplePage に戻る
        -}
    }

    transition exitButton_clicked
    --> {
        post {
             target state;
             state' = STOP;
        } @ {-
             アプリケーション終了する
        -}
    }
}
```

## イベントの定義

状態遷移イベントを定義する。

```
event = eventName1
      | eventName2 of Int
      | eventName3 of Int * String
```

文法は OCamlの Variantの借用。

## state定義

`state` 構文で定義するのは **プロセス** である。
特殊変数 `state` に 束縛できるのはプロセスのみである。

`state`には引数をもたせることができる。

```
state OkDialog (msg : String) -> {

    ...

}
```

`state` の引数は state内で参照できる

note: ページとかダイアログとかのようなものを `state` で定義します。

## 状態変数

`state` ブロック内に状態変数を作ることができる. 初期値を与えることができる。

```
var <variable_name> : <type_name> = <expression> ;

var <variable_name> = <expression> ;

var <variable_name> : <type_name> ;
```

例文中では冗長に書いたが、この場合型推論可能であるため
` : <type_name> ` は不要である。

状態変数は mutable である。


## invariant コンディションブロック

状態内不変条件である。

```
invariant {
} @ {-
-}
```

ここには状態内で不変となる条件を書く。
invariantコンディションに書いたからと行って
postコンディションに書かなくて良いということにはならない。

(postコンディションの検査用の条件と捉えるのが正しい）


invariant や post の後に続くブロックをコンディションブロックと呼ぶ

### コンディションブロック

このブロック内は述語である。全体で大きなブーリアンとなる。

* target 式
* `;` が使える

`target` 式 は、イベント前後で変更のある変数を列挙することで、
それ以外が変化しないことを示す 論理式である。
その性質から、主にpostコンディションの記述に確認する。

(本来は unchange 式で変化しない条件を列挙するほうが変数宣言のように見えたほうが馴染みやすいだろうとの配慮）

`;` は、論理積である。(`&&`と結合の優先順位は違う)

コンディションブロックは自然言語コメントブロックは必須。

## transition

外部選択による状態遷移を示す。

```
transition <event>
      when <formal-lang-predicate> @[- <natural-lang-predicate> -]
--> {
    post {
        <formal-lang-condition>
    }@{-
        <natural-lang-condition>
    -}
}
```
という形式を取る。

意味論としては CSPの

```
□ event → Process
```

### event式

`<event>` には、

* event定義に定義されたイベント
* channel 通信

が入る.eventは引数をとるものも定義できるので、
ここのパターンマッチでも分岐可能。

### when句

`when` 句はガード式となる。ガードによる分岐が必要ない場合は不要。

ここはコンディションブロックではないので、`target` も `;` 論理積も使用不能.

When句は自然言語コメントブロックは必須。

### transitionブロック

```
--> {
   <postコンディション>
}
```

あるいは、

```
--> {
   <postコンディション>
}@{-
   <自然言語のコメント>
-}
```

の形式をとる。この状態遷移に「事後条件」でないコメントをつけたい場合は、
自然言語コメントブロックをつけても良い。（すなわち、必須ではない）


### postコンディションブロック

キーワード`post` に続き コンディションブロックを取る

事前と事後の状態を比較するために、
事後の状態は、`変数名'`と表現できる。

特集変数 `state` は 定義したステートと比較する。
この `state` 変数が下記変わった時、別の 状態に遷移する（たとえばページ遷移）
