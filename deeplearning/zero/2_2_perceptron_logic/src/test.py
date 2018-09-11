#!/usr/bin/env python3

import numpy as np

def AND_simple(x1, x2):
    """
    2.3.1 簡単な実装

    y  = { 0 .. (w1x1 + w2x2 ≦ θ)
           1 .. (w1x1 + w2x2 > θ)
         }
    """
    w1, w2, theta = 0.5, 0.5, 0.7
    return 0 if (x1*w1 + x2*w2 < theta) else 1

def AND(x1, x2):
    """
    2.3.2 重みとバイアスの導入 / 重みとバイアスの実装

    y  = { 0 .. (b + w1x1 + w2x2 ≦ 0)
           1 .. (b + w1x1 + w2x2 > 0)
         }
    """
    x = np.array( [x1, x2] )
    w = np.array( [0.5, 0.5] )
    b = -0.7                     # bias

    cond = np.sum(w*x) + b # 行列の掛け算で実装
    return 0 if cond <= 0 else 1

def NAND(x1, x2):
    x = np.array( [x1, x2] )
    w = np.array( [-0.5, -0.5] )
    b = 0.7                     # bias

    cond = np.sum(w*x) + b # 行列の掛け算で実装
    return 0 if cond <= 0 else 1

def OR(x1, x2):
    x = np.array( [x1, x2] )
    w = np.array( [0.5, 0.5] )
    b = -0.2                     # bias

    cond = np.sum(w*x) + b # 行列の掛け算で実装
    return 0 if cond <= 0 else 1


def XOR(x1, x2):
    """
    multi-layerd perceptron

    L0                  L1                L2

    (x1)---+-----------> (s1)------+
           |   +-------> (  )      |
           |   |                   +----->(y)
           |   |                   +----->( )
           +---|-------> (  )      |
    (x2)-------+-------> (s2)------+
    """
    s1 = NAND(x1, x2)
    s2 = OR(x1,x2)
    return AND(s1, s2)

if __name__ == "__main__":
    def test_equal(title, cond, ans):
        print( "%s: test:\"%s\" .. %s " % ("OK" if cond == ans else "NG", title, cond))
        return cond == ans

    tests = [ test_equal( "AND 0 0", AND(0,0), 0),
              test_equal( "AND 0 1", AND(0,1), 0),
              test_equal( "AND 1 0", AND(1,0), 0),
              test_equal( "AND 1 1", AND(1,1), 1),
              test_equal( "NAND 0 0", NAND(0,0), 1),
              test_equal( "NAND 0 1", NAND(0,1), 1),
              test_equal( "NAND 1 0", NAND(1,0), 1),
              test_equal( "NAND 1 1", NAND(1,1), 0),
              test_equal( "OR 0 0", OR(0,0), 0),
              test_equal( "OR 0 1", OR(0,1), 1),
              test_equal( "OR 1 0", OR(1,0), 1),
              test_equal( "OR 1 1", OR(1,1), 1),
              test_equal( "XOR 0 0", XOR(0,0), 0),
              test_equal( "XOR 0 1", XOR(0,1), 1),
              test_equal( "XOR 1 0", XOR(1,0), 1),
              test_equal( "XOR 1 1", XOR(1,1), 0)
    ]
    print ( "  %d tests: OK %d   NG %d" % (len(tests), len([i for i in tests if i ]), len([i for i in tests if not i])))

