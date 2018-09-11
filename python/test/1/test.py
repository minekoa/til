### 行列の鞍関数を求める
### (エッジは無視するっぽい）
###
### 行の最大 かつ 列の最小
###
###      もしくは
###
### 行の最小 かつ 列の最大
###
### とのこと

def sol (A):
    return len( listupSaddlePoints(A) )

def listupSaddlePoints(A):
    s = set()

    for ri, row in enumerate( A ):
        if ri == 0 or ri == len(A) -1 : continue # edge clipping


        maxcol_idxs = getLocalMaximumIndexes(row)
        for ci in maxcol_idxs:
            col = getCol(ci, A)
            minrow_idxs = getLocalMinimumIndexes(col)

            if ri in minrow_idxs:
                s.add( (ri,ci) )
                log ("hit! (%i, %i) in %s %s" % (ri, ci
                                                , row[0:ci] + ["^%s" % row[ci] ] + row[ci+1:]
                                                , col[0:ri] + ["v%s" % col[ri] ] + col[ri+1:]))

        mincol_idxs = getLocalMinimumIndexes(row)
        for ci in mincol_idxs:
            col = getCol(ci, A)
            maxrow_idxs = getLocalMaximumIndexes(col)

            if ri in maxrow_idxs:
                s.add( (ri,ci) )
                log ("hit! (%i, %i) in %s %s" % (ri, ci
                                                , row[0:ci] + ["v%s" % row[ci] ] + row[ci+1:]
                                                , col[0:ri] + ["^%s" % col[ri] ] + col[ri+1:]))


    return s


def getLocalMaximumIndexes( lst ):
    ret = []
    for i in range(0, len(lst)):
        if i -1 < 0 or i +1 >= len(lst):
            continue

        if (lst[i -1] < lst[i]) and (lst[ i + 1] < lst[i]):
            ret.append(i)
    return ret
        
def getLocalMinimumIndexes( lst ):
    ret = []
    for i in range(0, len(lst)):
        if i -1 < 0 or i +1 >= len(lst):
            continue

        if (lst[i -1] > lst[i]) and (lst[i] < lst[i+1]):
            ret.append(i)
    return ret

                   
def getCol( n, lst ):
    return [ i[n] for i in lst ]
    

def log (str):
    pass
#    print str:

def visualize (A, ans):
    str = ""
    for ri, row in enumerate(A):
        for ci, cel in enumerate(row):
            if (ri,ci) in ans:
                str += "(%s) " % A[ri][ci]
            else:
                str += " %s  " % A[ri][ci]
        str += "\n"

    return str


if __name__ == "__main__":
    # 1,1 (v=5)と 1,2 (v=8)らしい 
    A=[[0,1,9,3],
       [7,5,8,3],
       [9,2,9,4],
       [4,6,7,1]]

    print( "saddle points count = %d" % sol(A) )
    print( visualize(A, listupSaddlePoints(A)))


