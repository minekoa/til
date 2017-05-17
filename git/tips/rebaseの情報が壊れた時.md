# rebaseの情報が壊れた時

`git rebase` した時にコンフリクトが生じ、その修正を `git add` した後、
うっかり (`git rebase --continue`ではなく) `git commit` してしまった場合など、
rebase の情報（途中までのキャッシュ？）が残ってしまい、rebaseが効かなくなったり、
意図せぬrebase（すでにpush済みの過去を巻き込んで改ざんしてしまう）ことがある。

例えばこんなエラー

```
It seems that there is already a rebase-apply directory, and
I wonder if you are in the middle of another rebase.  If that is the
case, please try
    git rebase (--continue | --abort | --skip)
If that is not the case, please
    rm -fr "/home/hogehoge/poyo-project/.git/rebase-apply"
and run me again.  I am stopping in case you still have something
valuable there.
```

rebase のための情報が .git の中に残ったままなのが問題なので、メッセージの通り、`rm -fr /dir/.git/rebase-apply` するなどすればいい。

* [githubでrebaseのエラー - 万年素人からHackerへの道](http://shinriyo.hateblo.jp/entry/2014/09/22/github%E3%81%A7rebase%E3%81%AE%E3%82%A8%E3%83%A9%E3%83%BC)
