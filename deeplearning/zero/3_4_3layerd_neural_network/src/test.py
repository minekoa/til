#!/usr/bin/env python3

import numpy as np

#  L0                L1                     L2                     L3
#  <X>          <A1>─h→<Z1>          <A2>─h→<Z2>          <A3>─σ→<Y>
#
#
#     b1 -+                   b2 -+
#         +---- ((a1)->(z1))  ----+                  b3 -+
#         |                       +---- ((a3)->(z3)) ----+
#  (x1) --+                       |                      +---- ((a5)->(y1))
#　　　　 +---- ((a2)->(z2))  ----+                      |
#         |                       +---- ((a4)->(z4)) ----+
#  (x2) --+                       |                      +---- ((a6)->(y2))
#　　　　 +---- ((a3)->(z3))  ----+



def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def id_func(x):
    return x


# imput ------------------------------------------------------------
X  = np.array( [1.0, 0.5] )


# layer 1 ----------------------------------------------------------

def layer1(X):
                                       # ┌                      ┐
    W1 = np.array( [[0.1, 0.3, 0.5],   # │a1←x1, a2←x1, a3←x1│
                    [0.2, 0.4, 0.6]] ) # │a1←x2, a2←x2, a3←x2│
                                       # └                      ┘
    B1 = np.array( [0.1, 0.2, 0.3] )
     
    A1 = np.dot(X, W1) + B1
    Z1 = sigmoid(A1)
    return Z1

# layer 2 ----------------------------------------------------------

def layer2(Z1):
                                  # ┌              ┐
    W2 = np.array( [[0.1, 0.4],   # │a3←a1, a4←a1│
                    [0.2, 0.5],   # │a3←a2, a4←a2│
                    [0.3, 0.6]] ) # │a3←a3, a4←a3│
                                  # └              ┘
    B2 = np.array( [0.1, 0.2] )
     
    A2 = np.dot(Z1, W2) + B2
    Z2 = sigmoid(A2)
    return Z2

# output ------------------------------------------------------------
def layer3(Z2):
                                  # ┌              ┐
    W3 = np.array( [[0.1, 0.3],   # │y1←a3, y1←a3│
                    [0.2, 0.4]] ) # │y1←a4, y1←a4│
                                  # └              ┘
    B3 = np.array( [0.1, 0.2] )
     
    A3 = np.dot(Z2, W3) + B3
    Y = id_func(A3)
    return Y


if __name__ == '__main__':
    Y = layer3( layer2( layer1( X ) ) )
    print ( Y )

