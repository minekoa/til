# gitでコミットしてしまった auther (user.name, user.email) を修正する

* [git でメールアドレスやら名前やらを間違えて commit してしまったときの修正方法Add StarClicheskyrisertamu222iwindish](http://d.hatena.ne.jp/naga_sawa/20110119/1295420861)

対話 rebase を起動 (`HEAD~3`は修正したいコミットのが何件前にいるかにあわせて適当に）

```
$ git rebase -i HEAD~3
```

テキストエディタが起動して、 `pick a11b22 コミットタイトルですよ` 的なリストが表示されるので、
修正したいコミットの行の頭の `pick` を `edit` に編集して保存する

すると

```
Stopped at e566fc3... relocate peripheral files
You can amend the commit now, with

        git commit --amend

Once you are satisfied with your changes, run

        git rebase --continue
```

なんてコンソールで言われるので

```
$ git commit --amend --author="minekoa <mineko.orange@gmail.com>"
```

して、`--amend` し、 `git rebase --continue` すればよし
