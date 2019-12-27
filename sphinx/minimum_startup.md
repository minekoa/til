# Sphinx を初めて見る

## マイルストーン

* sphinx のインストール
* reStructuredText の勉強
* ビルドしてみる

## sphinx のインストール


### pip のインストール

インストールに pip が必要なので、まずはpipから。

参考:
* [Ubuntu18.04でPython環境構築](https://yuta0508.hatenablog.com/entry/2018/05/06/104400)


```
$ sudo apt install python3-pip
```

```
$ pip3 --version
pip 9.0.1 from /usr/lib/python3/dist-packages (python 3.6)
```

```
$ sudo pip3 install --upgrade pip
The directory '/home/terazono/.cache/pip/http' or its parent directory is not owned by the current user and the cache has been disabled. Please check the permissions and owner of that directory. If executing pip with sudo, you may want sudo's -H flag.
The directory '/home/terazono/.cache/pip' or its parent directory is not owned by the current user and caching wheels has been disabled. check the permissions and owner of that directory. If executing pip with sudo, you may want sudo's -H flag.
Collecting pip
  Downloading https://files.pythonhosted.org/packages/c2/d7/90f34cb0d83a6c5631cf71dfe64cc1054598c843a92b400e55675cc2ac37/pip-18.1-py2.py3-none-any.whl (1.3MB)
    100% |████████████████████████████████| 1.3MB 425kB/s
Installing collected packages: pip
  Found existing installation: pip 9.0.1
    Not uninstalling pip at /usr/lib/python3/dist-packages, outside environment /usr
Successfully installed pip-18.1
```

### sphinx のインストール

```
$ sudo pip3 install sphinx
```
