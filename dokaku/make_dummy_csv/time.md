# 作業メモ

## 時間メモ

```
2018-10-30(火) 13:55:44 2018-10-30(火) 14:54:07 >   58:23
2018-10-30(火) 15:01:48 2018-10-30(火) 15:38:56 >   37:08
2018-10-30(火) 16:00:42 2018-10-30(火) 16:27:56 >   27:14
2018-10-30(火) 18:00:31 2018-10-30(火) 19:17:55 > 1:17:24
23:00頃                 2018-10-31(水) 00:33:13 > 1:33:13  // Q1 function 揃う
2018-10-31(水) 10:27:03 2018-10-31(水) 11:08:49 >   41:46
13:00 頃                2018-10-31(水) 15:25:51 > 2:25:51
16:00 頃　　　　　　　  2018-10-31(水) 16:18:08 >   18:08  // Q1 end
                                              sum 7:47:07
                                                  
2018-10-31(水) 18:21:41 2018-10-31(水) 18:54:47 >   33:06  // Q2-1 end
                                              sum   33:06

2018-10-31(水) 19:23:11 2018-10-31(水) 19:46:57 >   23:46  // Q2-ii python版（クソ重い）
20:10頃                 2018-10-31(水) 20:39:36 >   29:36  // Q2-ii python版 (パフォーマンスカイゼン)
2018-10-31(水) 22:10:12 2018-10-31(水) 23:00:25 >   50:13  // Q2-ii C++版
                                           sum    1:43:35
```

## 問題1 ダミーCSV作成

```
$ /usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./a.out 7000000
```

で、1.1Gくらいのファイルができる。
ユニークにする部分でどうしてもメモリを消費する。
この時 1,100,000 (KB) くらいなので、どうにかならないかなと思う。


### 作戦1 hash_mapを使う

hash_map の方が消費量少ないかな、と重い、


`std::set`版

```
1099132(KB) e=2:53.94 s=18.62 u=154.34
-rw-rw-r--. 1 zoni zoni 1.1G 10月 31 00:39 log.csv
```

`std::undorderd_set`版

```
1018664(KB) e=2:06.43 s=17.85 u=107.74
-rw-rw-r--. 1 zoni zoni 1.1G 10月 31 00:44 log.csv
```

ちょっとしか変わらず

あと、set と言っても、内部的には `hash_key -> value` になっているはずだけど、
値をとり出さないので、`unordered_map< string, char>` とすれば
事実上メモリ消費量減るかな、とおもったのだけれども、結果、全く変わらなかった

### 作戦2

文字の連結リストでユニーク判定すればいいかな、と思いついたけれど
仕様から間違っていた（部分文字列が一致すれば「衝突」してしまう）


### 作戦3

一文字を更に圧縮
あまり効果なし。あとプログラムミスった。
(特にメールアドレス側)

loginID (英語小文字Only)に特化させれば切り詰められるかもしれない
（3bit で1文字表現できるので、メモリ消費量が1/2くらいになるかも）


### 作戦4

一旦ファイルに書き出す。
効果はあるだろうな、と思ったのだけれども、かっこ悪い。
でもこれ以上時間を消費するのもアレなので。

まずはやる前に調査。

login_id, mailaddress ともにユニーク

```
1018904(KB) e=2:06.48 s=18.31 u=106.93
```

login id not uniq

```
512352(KB) e=1:42.11 s=15.30 u=85.79
```

mailaddress not uniq

```
512328(KB) e=1:53.47 s=17.52 u=95.05
```

いけそうなので、テスト実装


```
512368(KB) e=2:35.28 s=44.07 u=108.97
(xenial)zoni@localhost:~/work/reps/ymlsq/pj1/src$ ll -h
合計 1.3G
-rw-rw-r--. 1 zoni zoni    5 10月 30 16:28 #datetime.py#
drwxrwxr-x. 2 zoni zoni 4.0K 10月 31 15:20 ./
lrwxrwxrwx. 1 zoni zoni   20 10月 30 16:26 .#datetime.py -> zoni@localhost.30343
drwxrwxr-x. 4 zoni zoni 4.0K 10月 31 13:57 ../
-rw-rw-r--. 1 zoni zoni    7 10月 30 14:22 Makefile
-rw-rw-r--. 1 zoni zoni 2.4K 10月 31 14:42 TextEncoder.cpp
-rwxrwxr-x. 1 zoni zoni 416K 10月 31 15:20 a.out*
-rw-rw-r--. 1 zoni zoni 1.1G 10月 31 15:23 log.csv
-rw-rw-r--. 1 zoni zoni  47M 10月 31 15:21 loginid.tmp
-rw-rw-r--. 1 zoni zoni 139M 10月 31 15:21 mailaddr.tmp
-rw-rw-r--. 1 zoni zoni 9.2K 10月 31 15:20 main.cpp
-rw-rw-r--. 1 zoni zoni   25 10月 30 14:34 main.d
-rw-rw-r--. 1 zoni zoni 4.4K 10月 30 14:34 main.o
```

これでOKとする


### 最終版での計測

```
(xenial)zoni@localhost:~/work/reps/ymlsq/pj1/src$ /usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./a.out 7000000
512372(KB) e=2:31.33 s=42.53 u=106.66
(xenial)zoni@localhost:~/work/reps/ymlsq/pj1/src$ ll -h
合計 1.3G
-rw-rw-r--. 1 zoni zoni    5 10月 30 16:28 #datetime.py#
drwxrwxr-x. 2 zoni zoni 4.0K 10月 31 16:12 ./
lrwxrwxrwx. 1 zoni zoni   20 10月 30 16:26 .#datetime.py -> zoni@localhost.30343
drwxrwxr-x. 4 zoni zoni 4.0K 10月 31 13:57 ../
-rw-rw-r--. 1 zoni zoni    7 10月 30 14:22 Makefile
-rw-rw-r--. 1 zoni zoni 2.4K 10月 31 14:42 TextEncoder.cpp
-rwxrwxr-x. 1 zoni zoni 407K 10月 31 16:12 a.out*
-rw-rw-r--. 1 zoni zoni 1.1G 10月 31 16:15 log.csv
-rw-rw-r--. 1 zoni zoni  47M 10月 31 16:13 loginid.tmp
-rw-rw-r--. 1 zoni zoni 139M 10月 31 16:12 mailaddr.tmp
-rw-rw-r--. 1 zoni zoni  11K 10月 31 16:11 main.cpp
-rw-rw-r--. 1 zoni zoni   25 10月 30 14:34 main.d
-rw-rw-r--. 1 zoni zoni 4.4K 10月 30 14:34 main.o
```

## 問題2-i ドメイン毎の集計 (TOP 100)

こちらは普通に Python で実装しても問題無いはずなので、
C++ という苦行はやめた。

```
 /usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./aggregation_domain.py ../../pj1/log_1G.csv 
19324(KB) e=0:27.10 s=0.40 u=25.45
```

問題なかった。

## 問題2-ii ２つのファイルから、login ID が一致するものを抜き出す

愚直に一つのファイル一行づつに送りながら、もうひとつのファイルを全部舐める方式で
実装したが、死ぬほど遅かった（多分一日回しても終らない）

そこで、片方のファイルを先に舐め、 loginID -> seek_offset のマップを作り、
そのマップでマッピングすることにした。


### Python版

```
1443232(KB) e=2:26.96 s=14.62 u=77.33
```

時間はともかくメモリは 1,443,232KB なので、1GB以内をクリアできない。
うーん、これは予想外。


### C++版

ただ、問題1 の計測結果から、C++で掛けば 500MBくらいで行けるのはわかっているので、
さくっと書き直す。

```
$ /usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./a.out ../../pj1/log.csv ../../pj1/log_1G.csv 
509560(KB) e=2:19.84 s=6.63 u=80.34
```

クリアしたので良しとしよう。
