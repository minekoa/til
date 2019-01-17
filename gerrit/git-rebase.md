# Gerrit 運用における、複数commitの修正について

Gerrit は 1コミット = 1プルリクエストのような感じでレビューします。
コミットメッセージに Change-Id を含ませることで、レビューを認識するみたい。

レビューにたいして修正を行うときは、
修正コミットをつなげちゃだめで、git commit --amend する必要がありますが、
連続する修正commitの場合、結構面倒なことになりますよね


```
( ) change1 commit (ID:0000001)...指摘受けた!! どうする!! (amend すると change2 commit の親変わっちゃうよ!?)
 ^
 |
( ) change2 commit (ID:0000002)
 ^
 |
( ) change3 commit (ID:0000003)
```

こういった場合、git rebase を使うしかありません。
（余談: rebase したものを共通リポジトリに押し込むの、すげー怖い。 Gerritの設計頭おかしい)

## Step1 修正をコミットする

change1 のレビュー指摘についての修正をおこない、
普通にコミットしましょう

```
( ) change1 commit (ID:0000001)
 ^
 |
( ) change2 commit (ID:0000002)
 ^
 |
( ) change3 commit (ID:0000003)
 ^
 |
( ) fix review change1 (ID:0000004)
```

## Step2 `git rebase -i HEAD~4`

`git rebase -i HEAD~4` を実施します。

（`-i` は対話型 rebase ですね, `HEAD~4` は HEAD から４つ分の意味です）

コマンドを実行すると、エディタが起動して以下のような内容が表示されるので、

```
pick 0000001 change1 commit
pick 0000002 change2 commit
pick 0000003 change3 commit
pick 0000004 fix review change1

# Rebase 00000001..00000004 onto abcdef9
#
# Commands:
#  p, pick = use commit
#  r, reword = use commit, but edit the commit message
#  e, edit = use commit, but stop for amending
#  s, squash = use commit, but meld into previous commit
#  f, fixup = like "squash", but discard this commit's log message
#  x, exec = run command (the rest of the line) using shell
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
# However, if you remove everything, the rebase will be aborted.
#
```

以下のように、change1 commit のすぐ下に修正コミットの行を移動し`s` をつける編集し、

```
pick 0000001 change1 commit
s 0000004 fix review change1
pick 0000002 change2 commit
pick 0000003 change3 commit


# Rebase 00000001..00000004 onto abcdef9
#
# Commands:
#  p, pick = use commit
#  r, reword = use commit, but edit the commit message
#  e, edit = use commit, but stop for amending
#  s, squash = use commit, but meld into previous commit
#  f, fixup = like "squash", but discard this commit's log message
#  x, exec = run command (the rest of the line) using shell
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
# However, if you remove everything, the rebase will be aborted.
#
```
保存します。そうすると、 0000001 と 0000004 のコミットが一つにまとめられるので(`squash` :「押しつぶす」の意)、
Gerrit の「レビューに対する修正はCommitを生やさない（レビューコミット自身を修正）」の条件を満たします。

（このあと、コミットメッセージを編集するためにエディタがひらくので、決して ChangeID を変更しないように!)

結果、コミットツリーは以下のように改ざんされるわけです

```
( ) change1 commit (ID:0000005) ...ID0000001と0000004をくっつけたもの（つまりレビュー修正済みコミット)
 ^
 |
( ) change2 commit (ID:0000006) ...※親がかわってるので IDは変わってしまいます
 ^
 |
( ) change3 commit (ID:0000007) ...※同じく、親がかわってるので IDは変わってしまいます
```

これを `git review master` などで gerrit 側に送ってあげればよいわけです。
（ただし、change2, change3 も修正を送ったように見えてしまうのが玉に瑕)


## 参考

* [rebase -i でコミットをまとめる | Qiita](https://qiita.com/takke/items/3400b55becfd72769214)

