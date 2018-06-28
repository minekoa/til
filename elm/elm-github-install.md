# elm-github-installを使う

Nativeモジュールを含むと、現状パッケージとして公開できないため（結構ひどいことだと思う）
非公式インストーラーに頼るしか無い……・というわけで elm-github-install を使う。

https://github.com/gdotdesign/elm-github-install

（ちなみにこちらのパッケージで知った)

https://github.com/simonh1000/file-reader

> Due to the native (kernel) code, it is not possible to install directly using elm-package install. So you need on eof the following methods
>
> ### [elm-github-install](https://github.com/gdotdesign/elm-github-install)
>
> A tool to install native-code based Elm libraries


## インストール

いろんなインストール方法があるようだが、npmを使っていれた。

```console
$ npm install elm-github-install -g
```


## 叩いてみる

```console
$ elm-install 
Resolving packages...
  ▶ Package: https://github.com/elm-lang/core not found in cache, cloning...
  ▶ Package: https://github.com/elm-lang/html not found in cache, cloning...
  ▶ Package: https://github.com/elm-lang/virtual-dom not found in cache, cloning...
Solving dependencies...
  ● elm-lang/core - https://github.com/elm-lang/core (5.1.1)
  ● elm-lang/html - https://github.com/elm-lang/html (2.0.0)
  ● elm-lang/virtual-dom - https://github.com/elm-lang/virtual-dom (2.0.4)
Packages configured successfully!
```


## 実際に使ってみる

elm-text-editor のパッケージとしての動作確認をしてみる。

```json
{
    "version": "1.0.0",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "BSD3",
    "source-directories": [
        "."
    ],
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/core": "5.1.1 <= v < 6.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0",
        "minekoa/elm-text-editor": "1.0.0 <= v < 2.0.0"
    },
    "dependency-sources": {
        "minekoa/elm-text-editor": {
            "url": "git@github.com:minekoa/elm-text-editor",
            "ref": "master"
        }
    },

    "elm-version": "0.18.0 <= v < 0.19.0"
}
```

elm-text-editor はまだタグを打っていないので（APIが固まってないので）、`defendency-sources`を用いて masterを入れさせる。


```console
$ elm-install 
Resolving packages...
  ▶ Getting updates for: elm-lang/core
  ▶ Getting updates for: elm-lang/html
  ▶ Getting updates for: elm-lang/virtual-dom
  ▶ Package: git@github.com:minekoa/elm-text-editor not found in cache, cloning...
  ▶ Package: https://github.com/elm-lang/mouse not found in cache, cloning...
  ▶ Package: https://github.com/elm-lang/dom not found in cache, cloning...
Solving dependencies...
  ● elm-lang/core - https://github.com/elm-lang/core (5.1.1)
  ● elm-lang/html - https://github.com/elm-lang/html (2.0.0)
  ● minekoa/elm-text-editor - git@github.com:minekoa/elm-text-editor at master (1.0.0)
  ● elm-lang/mouse - https://github.com/elm-lang/mouse (1.0.1)
  ● elm-lang/dom - https://github.com/elm-lang/dom (1.1.1)
  ● elm-lang/virtual-dom - https://github.com/elm-lang/virtual-dom (2.0.4)
Packages configured successfully!
```

おお、上手く行った。


あとは、普通にelm-make する。
