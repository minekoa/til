# CrazyDiskInfoのインストール

https://github.com/otakuto/crazydiskinfo の README.md をそのまんまやればOK。

## 依存ライブラリを入れる

```
$ sudo apt install libatasmart-dev libncurses5-dev libncursesw5-dev
```

## GitHubからソースコードを取得

```
~/src$ git clone https://github.com/otakuto/CrazyDiskInfo.git
```

masterの最新 24c3b98421e4c96b684a84b576ddf2c815e7a6c6 をビルドした。1.0.2 のタグ打ちがしてあったので、そちらのほうが良かったかもしれない。


## make install

```
~/src/$ cd CrazyDiskInfo/
~/src/CrazyDiskInfo$ mkdir build
~/src/CrazyDiskInfo$ cd build/
~/src/CrazyDiskInfo/build$ cmake ..
-- The CXX compiler identification is GNU 5.4.0
-- Check for working CXX compiler: /usr/bin/c++
-- Check for working CXX compiler: /usr/bin/c++ -- works
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Configuring done
-- Generating done
-- Build files have been written to: /home/keijuterazono/src/CrazyDiskInfo/build
~/src/CrazyDiskInfo/build$ make
Scanning dependencies of target CrazyDiskInfo
[ 50%] Building CXX object CMakeFiles/CrazyDiskInfo.dir/main.cpp.o
[100%] Linking CXX executable crazy
[100%] Built target CrazyDiskInfo
keijuterazono@LR-25:~/src/CrazyDiskInfo/build$ sudo make install
[100%] Built target CrazyDiskInfo
Install the project...
-- Install configuration: ""
-- Installing: /usr/sbin/crazy
```

実行

```
$ sudo crazy
```

