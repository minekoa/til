# git でコミットしたものを後から複数に分割する

[gitでコミットしたものを後から複数のコミットに分割する方法](http://d.hatena.ne.jp/cakephper/20111011/1318303685)

1. `git rebase -i HEAD^`
2. 最新のコミットに `edit` をマークする
3. `git reset HEAD^`
4. あとは add からやり直せば良い
