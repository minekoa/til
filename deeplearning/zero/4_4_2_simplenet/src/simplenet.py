#!/usr/bin/env python
#-*- coding: utf-8 -*-

import sys, os

sys.path.append( os.path.dirname(os.path.abspath(__file__)) + '/../../lib')
from neuralnet_tools import sigmoid, softmax, cross_entropy_error, numerical_gradient

import numpy as np


class SimpleNet:
    def __init__(self):
        self.W = np.random.randn(2,3) # ガウス分布で初期化

    def predict(self, x):
        return np.dot(x, self.W)

    def loss(self, x, t):
        z = self.predict(x)
        y = softmax(z)
        loss = cross_entropy_error(y, t)

        return loss



if __name__ == "__main__":
    net = SimpleNet()
    print( "Weight Parameters: %s" % net.W )


    x = np.array([0.6, 0.9])
    p = net.predict(x)
    print("Predict: %s" % p)


    mx = np.argmax(p)
    print("Index of Maximum: %s" % mx)


    t = np.array([0,0,1]) # 正解ラベル
    los = net.loss(x,t)

    print("loss: %s" % los)


    dw = numerical_gradient( lambda dmy: net.loss(x,t),
                             net.W )

    print("dW: %s" % dw)


    
