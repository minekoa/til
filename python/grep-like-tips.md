# grep的な処理

リスト内包表記を使うことで簡潔にかける


[Pythonでファイル内の任意の文字列を含む行を抽出（grep的処理）](https://note.nkmk.me/python-grep-like/)

```python
with open(path) as f:
    return [(i, line) for i, line in enumerate(f.readlines()) if 'XXX' in line]
```
