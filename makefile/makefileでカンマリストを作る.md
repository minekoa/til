# makefileでカンマ・セパレーテッド・リストを作る

ユニットテストフレームワークに渡す引数のために、
ビルド対象オブジェクトのリスト、つまり

```
TESTS=q1_tests.beam \
    q1_tests.beam \
    q2_tests.beam \
    q3_tests.beam \
    q4_tests.beam \
    q5_tests.beam
```

みたいなのから、

```
q1_tests, q2_tests, q3_tests, q4_tests, q5_tests
```

みたいな文字列が作りたい。

実は正攻法ではあんまりうまく行かない（ターミネータじゃなくってセパレータだから）
で、結局SED先生に頼って, スペースを `, ` に置換する作戦。

make や shellの道具だと、
list(スペース区切りリスト)に対するmapとかfoldはできても、listのjoin がないのですよね。

```
TEST_NAMES=$(basename $(TESTS))
TEST_COMMA_LIST=$(shell echo $(TEST_NAMES) | sed -e 's/[ \f\n\r\t]\+/, /g')
```

前後にスペースがある恐れが恐ろしかったのだけれども、
（その場合はもうひと工夫必要があった）それは大丈夫だった。
