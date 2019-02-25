# Mono の Ubuntu 18.04LTSへのインストール

[Download -Stable | Mono]https://www.mono-project.com/download/stable/#download-lin-ubuntu

## 1. Add the Mono repository to your system

```console
sudo apt install gnupg ca-certificates
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list
sudo apt update
```

## 2. Install Mono

```
sudo apt install mono-devel
```

結構時間かかかる。その間にパッケージとその説明について訳してみよう。

| Package                 | Description                                               | Description+                                                                |
|-------------------------|-----------------------------------------------------------|-----------------------------------------------------------------------------|
| mono-devel              | コードをコンパイルする場合に必要                          |                                                                             |
| mono-complete           | 全てをインストールする.                                   | "assenbly not found" エラーのほとんどはこれで解決できる                     |
| mono-dbg                | Framework libraries の debugging symbolを取得するのに必要 | スタックトレースで行番号が出せる                                            |
| referenceassemblies-pcl | PCL コンパイルをサポートするために必要                    | "Framework not installed: .NETPortable"エラーのほとんどのケースを解決できる |
| ca-certificates-mono    | HTTPS接続用のSSL証明書を取得するのに必要                  | HTTPS接続に問題がある場合はこれをインストールするうう                       |
| mono-xsp4               | ASP.NET アプリケーションの実行に必要                      |                                                                             |


## 3. Verify Installation

> After the installation completed successfully, it's a good idea to
> run through the basic hello world examples on [this page](https://www.mono-project.com/docs/getting-started/mono-basics/)
> to verify Mono is working correctly.

Hello worldをかけと仰る。


[Mono Basics | Mono](https://www.mono-project.com/docs/getting-started/mono-basics/)

```
cs forVerify
make
```

でテストが全部実行される。（確認は目視なのでよろしく)


### 余計なの

ただ、このテストには GTK#2.0 と XSP4 が必要になる
GTK# を使いたい場合は、

```
sudo apt install gtk-sharp2
```

ASP.NET を使いたい場合は

```
sudo apt install mono-xsp4
```
