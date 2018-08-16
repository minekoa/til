

## プロジェクトの作成

```
$ cargo new --bin gcd
```

```
.
├── Cargo.toml
└── src
    └── main.rs
```

## ビルド、テストの実行

ビルドして実行

```
$ cargo run

   Compiling gcd v0.1.0 (file:///home/zoni/work/reps/til/rust/programmingrust/chap2/gcd)
warning: function is never used: `gcd`, #[warn(dead_code)] on by default
  --> src/main.rs:6:1
   |
6  | fn gcd(mut n: u64, mut m: u64) -> u64 {
   | ^

    Finished dev [unoptimized + debuginfo] target(s) in 1.24 secs
     Running `target/debug/gcd`
Hello, world!
```

ユニットテストを実行

```
$ cargo test
   Compiling gcd v0.1.0 (file:///home/zoni/work/reps/til/rust/programmingrust/chap2/gcd)
    Finished dev [unoptimized + debuginfo] target(s) in 0.76 secs
     Running target/debug/deps/gcd-fd9c3fc7a9ba7bfa

running 1 test
test test_gcd ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured
```


