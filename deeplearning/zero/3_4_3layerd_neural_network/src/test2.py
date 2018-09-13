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

def init_network():
    network = { 'W1' : np.array( [[0.1, 0.3, 0.5],
                                  [0.2, 0.4, 0.6]] ),
                'b1' : np.array( [0.1, 0.2, 0.3] ),

                'W2' : np.array( [[0.1, 0.4],
                                  [0.2, 0.5],
                                  [0.3, 0.6]] ),
                'b2' : np.array( [0.1, 0.2] ),

                'W3' : np.array( [[0.1, 0.3],
                                  [0.2, 0.4]] ),
                'b3' : np.array( [0.1, 0.2] )
    }
    return network
    
def forward(network, x):
    W1, W2, W3 = network['W1'], network['W2'], network['W3']
    b1, b2, b3 = network['b1'], network['b2'], network['b3']

    a1 = np.dot(x, W1) + b1
    z1 = sigmoid(a1)

    a2 = np.dot(z1, W2) + b2
    z2 = sigmoid(a2)

    a3 = np.dot(z2, W3) + b3
    y  = id_func(a3)

    return y


if __name__ == '__main__':
    network = init_network()
    x = np.array([1.0, 0.5])
    y = forward(network, x)
    print(y)
