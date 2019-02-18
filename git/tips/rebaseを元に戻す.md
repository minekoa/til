# rebase を元に戻す

rebase で s しちゃったりなどの履歴の改ざんを
やっぱりなしね、とする方法。

1. `git reflog` で戻したいHEADを探す
2. `git reset --hard 'HEAD@{n}'` で HEADを戻す

git reflog で HEADの置き換えのログが愚直に残っている。
rebase -i の場合は、1stepづつちゃんと残っているし、
checkout によるブランチ乗り換えのログも残ってる。
ここからrebaseをする前のHEADを探し出して、そこに `git reset` するだけの単純なメソッド。

`reset`は reflog の履歴書式(n個前) でも行けるけれど、もちろん直接hash打ち込んでもいい。

[git rebaseを間違えた時に元に戻す](https://gist.github.com/naokazuterada/9e45a937f7574ebf91a7)
