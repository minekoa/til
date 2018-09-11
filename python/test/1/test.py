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
    



if __name__ == "__main__":

    def test_equal( a, b):
        print ("%s : %s" %  (a == b, a) )
        return a==b

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


    # 1,1 (v=5)と 1,2 (v=8)らしい 
    A=[[0,1,9,3],
       [7,5,8,3],
       [9,2,9,4],
       [4,6,7,1]]

    tests = [ test_equal( getLocalMinimumIndexes( [0,1,2,1,3,0,1] ), [3,5] ),
              test_equal( getLocalMaximumIndexes( [0,1,2,1,3,0,1] ), [2,4] ),
              test_equal( getCol( 1, [ [1,2,3],
                                       [4,5,6],
                                       [7,8,9] ] ),
                          [2,5,8]
              ),
              test_equal( listupSaddlePoints( [ [1,2,1],
                                                [1,2,1],
                                                [1,2,1] ] ),
                          set()
              ),
              test_equal( listupSaddlePoints( [ [2,3,4,1],
                                                [1,2,3,1],
                                                [2,3,4,2] ] ),
                          {(1,2)}
              ),
              test_equal( listupSaddlePoints( [ [3,2,1,2],
                                                [4,3,2,3],
                                                [3,2,1,2] ] ),
                          {(1,2)}
              ),
              test_equal( listupSaddlePoints( [ [4,3,2,3],
                                                [3,2,1,2],
                                                [4,3,2,3] ] ),
                          set()
              ),
              test_equal( listupSaddlePoints( [ [1, 2 , 1 , 2 , 1 , 2],
                                                [2, 3 ,(2), 3 ,(2), 3],
                                                [1,(2), 1 ,(2), 1 , 2],
                                                [2, 3 , 2 , 3 , 2 , 3]
                                                ] ),
                          {(1,2), (2,1),(2,3),(1,4)}
              ),
              test_equal(listupSaddlePoints(A), { (1,1), (1,2) } ),
              test_equal(sol(A),2)
    ]
    print ("NG" if False in tests else "OK", "%d tests" % len(tests))


    print( visualize(A, listupSaddlePoints(A)))


