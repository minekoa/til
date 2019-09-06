# `sodo` ではリダイレクトをつかった書き込みができない

```
$ sudo "hogehogehoge" > /path/to/file.ext
```

はぱーみっしょんでないになる

解決法は man に書いてある

> To make a usage listing of the directories in the `/home`
> partition. Note that this runs the commands in a sub-shell to make
> the cd and file redirection work.
>
> ```
> $ sudo sh -c "cd /home ; du -s * | sort -rn > USAGE"
> ```

へー。
