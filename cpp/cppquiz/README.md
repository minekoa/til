# あめいじんぐ C++クイズ

https://twitter.com/zygoloid/status/1107740875671498752

> C++ quiz time! Without checking, what does this print (assume an LP64 / LLP64 system):
> 
> short a = 1;
> std::cout << sizeof(+a)["23456"] << std::endl;
>
> [ ] 1
> [ ] 3
> [ ] 4
> [ ] 6
>

```console
$ g++ main.cpp
$ ./a.out
1
```

なんで！？

`+a` すると `int` になるので `sizeof(+a)` は `4`.

で、`4["23456"]` は `"23456"[4]` とおなじだから、答えは `6` だ！…と思ったらまさかの `1`。


バラして確認してみるも

```c++
    std::cout << "sizeof(a)   ="<< sizeof(a) << std::endl;
    std::cout << "sizeof(+a)  ="<< sizeof(+a) << std::endl;
    std::cout << "4[\"23456\"]  =" << 4["23456"] << std::endl;
```

```console
sizeof(a)   =2
sizeof(+a)  =4
4["23456"]  =6
```

うーん、あっている。

なにをどうすれば `1` になるのかさっぱりわかんない
