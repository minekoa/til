# アドバンスドKML勉強会

## REDのプロセス

* GUI
* ScenarioPlayer
* WebAPI Server
* Software keyboard

## 同期イベントとチャネル通信

### CSP都の違い

内部のイベントを外部のイベントより優先する、という意味論で形式化しようと考えている

```
P = C ! x → d ? y → Q
          ↑
      このタイミングはどうなってる?
```

内部イベントが優先される



---


```
state' = |~| n <- {i | i : uint8 % i} & apiResponse!ReadAnswerPacket(ReadAnswer{ ack<-n, seqId<-seq, size<-0, data<- [] }) -> state;
```

```
P = 1 → P
 □ 2 → P
 □ 3 → P
 □ 4 → P
 □ 5 → P

 ・・・
 
 ┌┐ m : 1 ≦ n ≦ 255 → P
```

