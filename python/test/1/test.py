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
    return len(listup(A))

def listup(A):
    s = set()

    for ri, row in enumerate( A ):
        if ri == 0 or ri == len(A) -1 : continue # edge clipping

        maxcol_idxs = getMaxIndexes(row)

        for ci in maxcol_idxs:
            col = getCol(ci, A)
            if col[ri] == min(col[1:-1]):
                s.add( (ri,ci) )
                log ("hit! (%i, %i) in %s %s" % (ri, ci
                                                 , row[0:ci] + ["^%s" % row[ci] ] + row[ci+1:]
                                                 , col[0:ri] + ["v%s" % col[ri] ] + col[ri+1:]))

        mincol_idxs = getMinIndexes(row)
        for ci in mincol_idxs:
            col = getCol(ci, A)
            if col[ri] == max(col[1:-1]):
                s.add( (ri,ci) )
                log ("hit! (%i, %i) in %s %s" % (ri, ci
                                                 , row[0:ci] + ["v%s" % row[ci] ] + row[ci+1:]
                                                 , col[0:ri] + ["^%s" % col[ri] ] + col[ri+1:]))


    return s


def getMaxIndexes( lst ):
    v = max(lst[1:-1])
    return [ i[0] for i in enumerate(lst) if i[1] == v and i[0] != 0 and i[0] != len(lst) -1 ]
        
def getMinIndexes( lst ):
    v = min(lst[1:-1])
    return [ i[0] for i in enumerate(lst) if i[1] == v and i[0] != 0 and i[0] != len(lst) -1 ]
                   
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
    print( visualize(A, listup(A)))




