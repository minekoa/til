# git でコミットしたものを後から複数に分割する

[gitでコミットしたものを後から複数のコミットに分割する方法](http://d.hatena.ne.jp/cakephper/20111011/1318303685)

1. `git rebase -i HEAD^`
2. 最新のコミットに `edit` をマークする
3. `git reset HEAD^`
4. あとは add -> commit を任意単位でやる
5. `git rebase --continue`

## 例

### 1.

```
$ git rebase -i HEAD^
```

### 2.

```
pick ca213369 Bug #25951
```

→

```
edit ca213369 Bug #25951
```

```
Stopped at ca213369...  Bug #25951
You can amend the commit now, with

  git commit --amend

Once you are satisfied with your changes, run

  git rebase --continue
```

### 3.

```
$ git reset HEAD^
Unstaged changes after reset:
M	doc/r_enq_error.rst
M	doc/rest-api/enquete.rst
M	src/r_enq_enquete.erl
M	src/r_enq_error.hrl
M	src/r_enq_webui_enquete_resolve_uri_priv.erl
M	test/rest-api/enquete_resolve_uri.js
```

### 4.

```
$ git add src/r_enq_enquete.erl
$ git add src/r_enq_error.hrl
$ git add src/r_enq_webui_enquete_resolve_uri_priv.erl
$ git commit
[detached HEAD b5187f0f] Bug #25951; Implement
 3 files changed, 90 insertions(+), 75 deletions(-)
```

```
$ git add test/rest-api/enquete_resolve_uri.js
$ git commit
[detached HEAD 2b2b8087] Bug #25951; Tests
 1 file changed, 162 insertions(+)
```

```
terazono@terazono-VirtualBox:~/reps/ridill-docker/ridill-code/enquete$ git add doc/r_enq_error.rst
terazono@terazono-VirtualBox:~/reps/ridill-docker/ridill-code/enquete$ git add doc/rest-api/enquete.rst terazono@terazono-VirtualBox:~/reps/ridill-docker/ridill-code/enquete$ git commit
[detached HEAD dfd17b46] Bug #25951; Documents
 2 files changed, 11 insertions(+)
```

### 5.

```
terazono@terazono-VirtualBox:~/reps/ridill-docker/ridill-code/enquete$ git rebase --continue
Successfully rebased and updated refs/heads/feature-bugfix-25951-resolve-uri-500-error-by-invalid-type.
```
