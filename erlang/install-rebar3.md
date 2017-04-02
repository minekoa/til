# rebar3 のインストール

まずは、リポジトリーをクローン

```
$ git clone https://github.com/erlang/rebar3.git
Cloning into 'rebar3'...
remote: Counting objects: 16550, done.
remote: Compressing objects: 100% (20/20), done.
remote: Total 16550 (delta 1), reused 0 (delta 0), pack-reused 16528
Receiving objects: 100% (16550/16550), 4.61 MiB | 1.25 MiB/s, done.
Resolving deltas: 100% (11315/11315), done.
Checking connectivity... done.
```

`rebar3`に入り、`./bootstrap` を実行します。

```
$ cd rebar3/
$ ./bootstrap 
===> Updating package registry...
===> Writing registry to /home/hoge/.cache/rebar3/hex/default/registry
===> Generating package index...
===> Writing index to /home/hoge/.cache/rebar3/hex/default/packages.idx
===> Verifying dependencies...
===> Fetching bbmustache ({pkg,<<"bbmustache">>,<<"1.3.0">>})
===> Downloaded package, caching at /home/hoge/.cache/rebar3/hex/default/packages/bbmustache-1.3.0.tar
===> Linking _build/default/lib/bbmustache to _build/bootstrap/lib/bbmustache
===> Linking _build/default/lib/certifi to _build/bootstrap/lib/certifi
===> Linking _build/default/lib/cf to _build/bootstrap/lib/cf
===> Fetching cth_readable ({pkg,<<"cth_readable">>,<<"1.2.4">>})
===> Downloaded package, caching at /home/hoge/.cache/rebar3/hex/default/packages/cth_readable-1.2.4.tar
===> Linking _build/default/lib/cth_readable to _build/bootstrap/lib/cth_readable
===> Linking _build/default/lib/erlware_commons to _build/bootstrap/lib/erlware_commons
===> Fetching eunit_formatters ({pkg,<<"eunit_formatters">>,
                                        <<"0.3.1">>})
===> Downloaded package, caching at /home/hoge/.cache/rebar3/hex/default/packages/eunit_formatters-0.3.1.tar
===> Linking _build/default/lib/eunit_formatters to _build/bootstrap/lib/eunit_formatters
===> Linking _build/default/lib/getopt to _build/bootstrap/lib/getopt
===> Linking _build/default/lib/providers to _build/bootstrap/lib/providers
===> Fetching relx ({pkg,<<"relx">>,<<"3.22.4">>})
===> Downloaded package, caching at /home/hoge/.cache/rebar3/hex/default/packages/relx-3.22.4.tar
===> Linking _build/default/lib/relx to _build/bootstrap/lib/relx
===> Fetching ssl_verify_fun ({pkg,<<"ssl_verify_fun">>,<<"1.1.1">>})
===> Downloaded package, caching at /home/hoge/.cache/rebar3/hex/default/packages/ssl_verify_fun-1.1.1.tar
===> Linking _build/default/lib/ssl_verify_fun to _build/bootstrap/lib/ssl_verify_fun
===> Compiling getopt
===> Compiling providers
===> Compiling cf
===> Compiling erlware_commons
===> Compiling bbmustache
===> Compiling relx
===> Compiling eunit_formatters
===> Compiling cth_readable
===> Compiling certifi
===> Compiling ssl_verify_fun
===> Compiling rebar
===> Cleaning out bbmustache...
===> Cleaning out certifi...
===> Cleaning out cf...
===> Cleaning out cth_readable...
===> Cleaning out erlware_commons...
===> Cleaning out eunit_formatters...
===> Cleaning out getopt...
===> Cleaning out providers...
===> Cleaning out rebar...
===> Cleaning out relx...
===> Cleaning out ssl_verify_fun...
===> Verifying dependencies...
===> Compiling getopt
===> Compiling providers
===> Compiling cf
===> Compiling erlware_commons
===> Compiling bbmustache
===> Compiling relx
===> Compiling eunit_formatters
===> Compiling cth_readable
===> Compiling certifi
===> Compiling ssl_verify_fun
===> Compiling rebar
===> Building escript...
```

インストールします。

```
$ ./rebar3 local install
===> Extracting rebar3 libs to /home/hoge/.cache/rebar3/lib...
===> Writing rebar3 run script /home/hoge/.cache/rebar3/bin/rebar3...
===> Add to $PATH for use: export PATH=$PATH:/home/hoge/.cache/rebar3/bin
```

あとは、`.bashrc` とか `.profile` とかに適当なところでパスを通しましょう
