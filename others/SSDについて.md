
## SLC / MLC / TLC

* [SSDのSLCとMLCとTLCの違いとは](http://kaworu.jpn.org/pc/SSD%E3%81%AESLC%E3%81%A8MLC%E3%81%A8TLC%E3%81%AE%E9%81%95%E3%81%84%E3%81%A8%E3%81%AF)
* [Understanding Flash: SLC, MLC and TLC](https://flashdba.com/2014/07/03/understanding-flash-slc-mlc-and-tlc/)

## SSD の寿命

* [SSD耐久テスト 裏側#1 大間違いのSSD寿命](http://eeepc.dnki.co.jp/?eid=1106886)
* [SSDの寿命の考え方と加速寿命テスト](http://xn--ssd-hz3g941m.com/?p=420)


### 寿命とはなにか

> さて、このようなわけで現実にはNANDフラッシュは内容がこぼれて無くならないよう、定期的に水を足す必要がある。
> DRAMと同じようにリフレッシュする必要があるわけだ。後述するウェアレベリング（摩耗平滑化）という機能がリフレッシュを兼ねている。
> 
> NANDフラッシュの寿命の有無とは、この定期的に継ぎ足す時間が十分長いか否かと言える。

であるため、SSDがROM化した時も、データが蒸発すると考えて、急いでデータバックアップをとらねばならない

> ROM化してもそのデータ保持時間は有限だと注意が必要だ。
> NANDフラッシュは磨耗するごとにデータの保持時間が短くなる。ROM化した後もどんどんデータは揮発していく事になる。
> 
> 一日６GBの書き込みをしている人であれば１２０GBのSSDで２０日でSSDのリフレッシュが一巡すると述べた。
> とすれば２０日のリフレッシュサイクルを保持できないNANDフラッシュが大量に出た結果と考えれば
> 単純計算で毎日６GBのデータが揮発していくと想像できる。

### リフレッシュ

> 現在のSSDにはウェアレベリング（摩耗平滑化）と 言って、内部のメモリ素子を満遍なく均一に使用する機能が入っている。
> この結果、SSDはそれぞれのNANDフラッシュメモリに均等に書き込みする。
> ウェアレベリングで書き込むNANDフラッシュメモリに既にいるデータは読み取り、他のメモリに退避書き込みするので、
> このタイミングでリフレッシュが起きる。
> 
> ＳＳＤで言う寿命が尽きるとは、このようにリフレッシュが間に合わなくなった状態と言える。

ウェアレベリングがリフレッシュを兼ねているとしたら、
ReadOnly 運用したSSDの寿命はどうなるのだろう。

### 寿命の指標

TBW () が指標と言える。ただし、論理書き込み容量と一致しない。これはブロックごとの削除を行うためである。

WAF(書き込み増幅率)を

## Linux で S.M.A.R.T.を使う

* [S.M.A.R.T | archlinux](https://wiki.archlinuxjp.org/index.php/S.M.A.R.T.)

### インストール

```
$ sudo apt install smartmontools
```

#### 1. Disk情報を得る

```
$ sudo smartctl --scan
/dev/sda -d scsi # /dev/sda, SCSI device
```

#### 2. S.M.A.R.T.が有効なのか、確認する

```
$ sudo smartctl /dev/sda -i
smartctl 6.5 2016-01-24 r4214 [x86_64-linux-4.4.0-64-generic] (local build)
Copyright (C) 2002-16, Bruce Allen, Christian Franke, www.smartmontools.org

=== START OF INFORMATION SECTION ===
Device Model:     SAMSUNG MZHPU128HCGM-000H1
Serial Number:    S1L3NYAG704710
LU WWN Device Id: 5 002538 655584d30
Firmware Version: UXM62H1Q
User Capacity:    128,035,676,160 bytes [128 GB]
Sector Size:      512 bytes logical/physical
Rotation Rate:    Solid State Device
Device is:        Not in smartctl database [for details use: -P showall]
ATA Version is:   ACS-2, ATA8-ACS T13/1699-D revision 4c
SATA Version is:  SATA 3.0, 6.0 Gb/s (current: 6.0 Gb/s)
Local Time is:    Mon Mar 27 13:49:16 2017 JST
SMART support is: Available - device has SMART capability.
SMART support is: Enabled
```

#### 3. S.M.A.R.T. 情報を得る
```
$ sudo smartctl /dev/sda -A
smartctl 6.5 2016-01-24 r4214 [x86_64-linux-4.4.0-64-generic] (local build)
Copyright (C) 2002-16, Bruce Allen, Christian Franke, www.smartmontools.org

=== START OF READ SMART DATA SECTION ===
SMART Attributes Data Structure revision number: 1
Vendor Specific SMART Attributes with Thresholds:
ID# ATTRIBUTE_NAME          FLAG     VALUE WORST THRESH TYPE      UPDATED  WHEN_FAILED RAW_VALUE
  9 Power_On_Hours          0x0032   097   097   000    Old_age   Always       -       10116
 12 Power_Cycle_Count       0x0032   099   099   000    Old_age   Always       -       176
177 Wear_Leveling_Count     0x0013   094   094   017    Pre-fail  Always       -       205
178 Used_Rsvd_Blk_Cnt_Chip  0x0013   087   087   010    Pre-fail  Always       -       164
179 Used_Rsvd_Blk_Cnt_Tot   0x0013   088   088   010    Pre-fail  Always       -       314
180 Unused_Rsvd_Blk_Cnt_Tot 0x0013   088   088   010    Pre-fail  Always       -       2310
183 Runtime_Bad_Block       0x0013   100   100   010    Pre-fail  Always       -       0
```

または、

```
$ sudo smartctl -a /dev/sda
smartctl 6.5 2016-01-24 r4214 [x86_64-linux-4.4.0-64-generic] (local build)
Copyright (C) 2002-16, Bruce Allen, Christian Franke, www.smartmontools.org

=== START OF INFORMATION SECTION ===
Device Model:     SAMSUNG MZHPU128HCGM-000H1
Serial Number:    S1L3NYAG704710
LU WWN Device Id: 5 002538 655584d30
Firmware Version: UXM62H1Q
User Capacity:    128,035,676,160 bytes [128 GB]
Sector Size:      512 bytes logical/physical
Rotation Rate:    Solid State Device
Device is:        Not in smartctl database [for details use: -P showall]
ATA Version is:   ACS-2, ATA8-ACS T13/1699-D revision 4c
SATA Version is:  SATA 3.0, 6.0 Gb/s (current: 6.0 Gb/s)
Local Time is:    Mon Mar 27 14:34:02 2017 JST
SMART support is: Available - device has SMART capability.
SMART support is: Enabled

=== START OF READ SMART DATA SECTION ===
SMART overall-health self-assessment test result: PASSED

General SMART Values:
Offline data collection status:  (0x00)	Offline data collection activity
					was never started.
					Auto Offline Data Collection: Disabled.
Self-test execution status:      (   0)	The previous self-test routine completed
					without error or no self-test has ever 
					been run.
Total time to complete Offline 
data collection: 		(    0) seconds.
Offline data collection
capabilities: 			 (0x5f) SMART execute Offline immediate.
					Auto Offline data collection on/off support.
					Abort Offline collection upon new
					command.
					Offline surface scan supported.
					Self-test supported.
					No Conveyance Self-test supported.
					Selective Self-test supported.
SMART capabilities:            (0x0003)	Saves SMART data before entering
					power-saving mode.
					Supports SMART auto save timer.
Error logging capability:        (0x01)	Error logging supported.
					General Purpose Logging supported.
Short self-test routine 
recommended polling time: 	 (   2) minutes.
Extended self-test routine
recommended polling time: 	 (  10) minutes.
SCT capabilities: 	       (0x003d)	SCT Status supported.
					SCT Error Recovery Control supported.
					SCT Feature Control supported.
					SCT Data Table supported.

SMART Attributes Data Structure revision number: 1
Vendor Specific SMART Attributes with Thresholds:
ID# ATTRIBUTE_NAME          FLAG     VALUE WORST THRESH TYPE      UPDATED  WHEN_FAILED RAW_VALUE
  9 Power_On_Hours          0x0032   097   097   000    Old_age   Always       -       10117
 12 Power_Cycle_Count       0x0032   099   099   000    Old_age   Always       -       176
177 Wear_Leveling_Count     0x0013   094   094   017    Pre-fail  Always       -       205
178 Used_Rsvd_Blk_Cnt_Chip  0x0013   087   087   010    Pre-fail  Always       -       164
179 Used_Rsvd_Blk_Cnt_Tot   0x0013   088   088   010    Pre-fail  Always       -       314
180 Unused_Rsvd_Blk_Cnt_Tot 0x0013   088   088   010    Pre-fail  Always       -       2310
183 Runtime_Bad_Block       0x0013   100   100   010    Pre-fail  Always       -       0

SMART Error Log Version: 1
No Errors Logged

SMART Self-test log structure revision number 1
Num  Test_Description    Status                  Remaining  LifeTime(hours)  LBA_of_first_error
# 1  Extended offline    Completed without error       00%       148         -
# 2  Short offline       Completed without error       00%       147         -
# 3  Short offline       Completed without error       00%         0         -
# 4  Short offline       Completed without error       00%         0         -

SMART Selective self-test log data structure revision number 1
 SPAN  MIN_LBA  MAX_LBA  CURRENT_TEST_STATUS
    1        0        0  Not_testing
    2        0        0  Not_testing
    3        0        0  Not_testing
    4        0        0  Not_testing
    5        0        0  Not_testing
Selective self-test flags (0x0):
  After scanning selected spans, do NOT read-scan remainder of disk.
If Selective self-test is pending on power-up, resume after 0 minute delay.
```


SMART の各項目の意味はこちら。

* [Self-Monitoring, Analysis and Reporting Technology - Wikipedia](https://ja.wikipedia.org/wiki/Self-Monitoring,_Analysis_and_Reporting_Technology)

