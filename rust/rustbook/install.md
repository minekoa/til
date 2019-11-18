# Install

```
$ curl https://sh.rustup.rs -sSf | sh
info: downloading installer

Welcome to Rust!

This will download and install the official compiler for the Rust
programming language, and its package manager, Cargo.

It will add the cargo, rustc, rustup and other commands to
Cargo's bin directory, located at:

  /home/zoniathena/.cargo/bin

This can be modified with the CARGO_HOME environment variable.

Rustup metadata and toolchains will be installed into the Rustup
home directory, located at:

  /home/zoniathena/.rustup

This can be modified with the RUSTUP_HOME environment variable.

This path will then be added to your PATH environment variable by
modifying the profile file located at:

  /home/zoniathena/.profile

You can uninstall at any time with rustup self uninstall and
these changes will be reverted.

Current installation options:


   default host triple: x86_64-unknown-linux-gnu
     default toolchain: stable
               profile: default
  modify PATH variable: yes

1) Proceed with installation (default)
2) Customize installation
3) Cancel installation
>1

info: profile set to 'default'
info: syncing channel updates for 'stable-x86_64-unknown-linux-gnu'
info: latest update on 2019-11-07, rust version 1.39.0 (4560ea788 2019-11-04)
info: downloading component 'cargo'
info: downloading component 'clippy'
info: downloading component 'rust-docs'
 11.8 MiB /  11.8 MiB (100 %)  10.6 MiB/s in  1s ETA:  0s
info: downloading component 'rust-std'
176.4 MiB / 176.4 MiB (100 %)  10.3 MiB/s in 22s ETA:  0s
info: downloading component 'rustc'
 66.3 MiB /  66.3 MiB (100 %)  11.8 MiB/s in  8s ETA:  0s
info: downloading component 'rustfmt'
info: installing component 'cargo'
info: installing component 'clippy'
info: installing component 'rust-docs'
 11.8 MiB /  11.8 MiB (100 %) 980.8 KiB/s in 12s ETA:  0s
info: installing component 'rust-std'
176.4 MiB / 176.4 MiB (100 %)   6.4 MiB/s in 24s ETA:  0s
info: installing component 'rustc'
 66.3 MiB /  66.3 MiB (100 %)   5.2 MiB/s in 14s ETA:  0s
info: installing component 'rustfmt'
info: default toolchain set to 'stable'

  stable installed - rustc 1.39.0 (4560ea788 2019-11-04)


Rust is installed now. Great!

To get started you need Cargo's bin directory ($HOME/.cargo/bin) in your PATH
environment variable. Next time you log in this will be done
automatically.

To configure your current shell run source $HOME/.cargo/env
```

```
$ source $HOME/.cargo/env
```

```
$ rustc --version
rustc 1.39.0 (4560ea788 2019-11-04)
```
