# cargo で hello world

[はじめる](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/getting-started.html)


### ディレクトリを作成する

こんな感じ

```
hello_cargo
├── Cargo.toml ..これから作るよ!
└── src
     └── main.rs
```

main.rs

```
fn main () {
    println! ("hello workd!");
}
```


### Cargo.toml ファイルを作る

先頭の `C` は大文字であることに注意

```
[package]

name = "hello"
version = "0.0.1"
authores = ["minekoa <mineko.orange@gmail.com>"]
```

### ビルドを実施

```
$ cargo build
warning: unused manifest key: package.authores
   Compiling hello v0.0.1 (file:///home/hoge/projects/til/rust/src/hello_cargo)
    Finished debug [unoptimized + debuginfo] target(s) in 0.39 secs
```

ビルドするとこんな感じ

```
hello_cargo
├── Cargo.lock
├── Cargo.toml
├── src
│   └── main.rs
└── target
    └── debug
        ├── build
        ├── deps
        │   └── hello-3710de52bed251be
        ├── examples
        ├── hello
        └── native
```

実行

```
$ ./target/debug/hello_cargo 
hello workd!
```

#### リリースビルド

リリースビルドするには `cargo build --release` すればいい

```
$ cargo build --release
warning: unused manifest key: package.authores
   Compiling hello v0.0.1 (file:///home/hoge/projects/til/rust/src/hello_cargo)
    Finished release [optimized] target(s) in 0.22 secs
$ tree
.
├── Cargo.lock
├── Cargo.toml
├── src
│   └── main.rs
└── target
    ├── debug
    │   ├── build
    │   ├── deps
    │   │   └── hello-3710de52bed251be
    │   ├── examples
    │   ├── hello
    │   └── native
    └── release
        ├── build
        ├── deps
        │   └── hello-ff2b59b6de67fb86
        ├── examples
        ├── hello
        └── native
```

## さいごに

いちいち手で作らなくても `cargo new hello_cargo --bin` のようにすれば作ってくれるよ。

* [Cargo Guilde](http://doc.crates.io/guilde.html)
* [Cargoの使い方](http://keens.github.io/blog/2015/11/29/cargonotsukaikata/)
