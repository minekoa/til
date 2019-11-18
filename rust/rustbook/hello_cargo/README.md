# hello cargo

```
$ cargo --version
cargo 1.39.0 (1c6ec66d5 2019-09-30)
```

```
$ cargo new hello_cargo --bin
```

```
$ cd hello_cargo
$ tree
.
├── Cargo.toml
└── src
    └── main.rs

1 directory, 2 files
```

## cargo build

```
$ cargo new hello_cargo --bin
drwxr-xr-x 1 zoniathena zoniathena  42 11月 18 22:32 target
```

```
$ tree
.
├── Cargo.lock
├── Cargo.toml
├── README.md
├── src
│   └── main.rs
└── target
    └── debug
        ├── build
        ├── deps
        │   ├── hello_cargo-e7064ff92467904d
        │   └── hello_cargo-e7064ff92467904d.d
        ├── examples
        ├── hello_cargo
        ├── hello_cargo.d
        └── incremental
            └── hello_cargo-fst4d3ecr5rw
                ├── s-fhys6uzts0-1eiu3h0-1c9cq6ydsy4rc
                │   ├── 1iy936wp4lwdotcc.o
                │   ├── 1j47aaombiw9p8jg.o
                │   ├── 1whyjx1ce30ghjm7.o
                │   ├── 3goavsngfncearxu.o
                │   ├── 4nbmlidi5k7ycjsd.o
                │   ├── 5008vtti6lkye9tl.o
                │   ├── dep-graph.bin
                │   ├── query-cache.bin
                │   └── work-products.bin
                └── s-fhys6uzts0-1eiu3h0.lock

9 directories, 18 files
```

```
$ ./target/debug/hello_cargo
Hello, world!
```

## cargo run

```
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.04s
     Running `target/debug/hello_cargo`
Hello, world!
```


## cargo check
```
$ cargo check
    Checking hello_cargo v0.1.0 (/home/zoniathena/reps/til/rust/rustbook/hello_cargo)
    Finished dev [unoptimized + debuginfo] target(s) in 0.32s
```
