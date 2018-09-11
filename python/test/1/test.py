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
    s = []

    for ri, row in enumerate( A ):
        if ri == 0 or ri == len(A) -1 : continue # edge clipping

        maxcol_idxs = getLocalMaximumIndexes(row)
        s += [ (ri, ci) for ci in maxcol_idxs if isLocalMinimumIn(ri, getCol(ci, A)) ] 

        mincol_idxs = getLocalMinimumIndexes(row)
        s += [ (ri, ci) for ci in mincol_idxs if isLocalMaximumIn(ri, getCol(ci, A)) ] 

    return set(s)


def getLocalMaximumIndexes( lst ):
    return [i for i in range(1, len(lst) -1) if lst[i-1] < lst[i] and lst[i] > lst[i+1]]

def isLocalMaximumIn(i, lst):
    return i in getLocalMaximumIndexes(lst)
        
def getLocalMinimumIndexes( lst ):
    return [i for i in range(1, len(lst) -1) if lst[i-1] > lst[i] and lst[i] < lst[i+1]]

def isLocalMinimumIn(i, lst):
    return i in getLocalMinimumIndexes(lst)
                   
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


