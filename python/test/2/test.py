### エレベータの停止回数を求める
### 地上階にいる 人の列を何回の停止で運べるか
###    * 列に並んでいる人は先頭から順にのせる
###    * エレベータのキャパシティ (人数 X or 重量 Y)いっぱいまでのせる
###    * 最後に地上階に戻ってくるのも 1回と数える
###    * 階数は0オリジン (0は地上階)

def sol(A,B,M,X,Y):
    assert( max(B) <= M )

    print ("%s %s: capacity %s people, %s kg" % (A, B, X, Y))
    return solrec(A,B, X,Y, 0)



def solrec( p_weight, p_goto, maxp, maxw, stopcnt ):
    if p_weight == []:
        return stopcnt

    takes = 0
    for i in range(0, len( p_weight)):
        if (maxp < i+1)  or (maxw < sum( p_weight[0:i+1]) ): break
        takes += 1
    assert( takes > 0 )

    stops = len( set(p_goto[0:takes]) ) +1

    print ("  %s %s" % (p_weight, p_goto))
    print( "    --> takes %d (%s kg): goto %s (stop %d)" % (takes,sum( p_weight[0:takes] ), p_goto[0:takes], stops))

    return solrec(p_weight[takes:], p_goto[takes:], maxp, maxw, stops + stopcnt)




print ("\n\ntest1 ------------------------------------------------------------")
ans = sol( [60,80,40], [2,3,5], 5, 2, 200 )
print("ans= %s" % ans )

print ("\n\ntest2 ------------------------------------------------------------")
ans2 = sol( [40, 40, 100, 80, 20], [3, 3, 2,2,3], 3, 5, 200)
print("ans2= %s" % ans2 )
