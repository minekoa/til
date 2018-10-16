#!/usr/bin/env python3
import sys, os
import pickle

sys.path.append( os.path.dirname(os.path.abspath(__file__)) + '/../../dataset')
from mnist import load_mnist

sys.path.append( os.path.dirname(os.path.abspath(__file__)) + '/../../lib')
from neuralnet_tools import sigmoid, softmax

import numpy as np

def get_data():
    (x_train, t_train), (x_test, t_test) = load_mnist(flatten=True,        # 一次元配列にする
                                                      normalize=True,      # データの範囲を 0.0 .. 1.0 にする
                                                      one_hot_label=False)
    return x_test, t_test

def init_network():
    with open("sample_weight.pkl", "rb") as f:
        network = pickle.load(f)
    return network

def predict(network, x):
    W1, W2, W3 = network['W1'], network['W2'], network['W3'] 
    b1, b2, b3 = network['b1'], network['b2'], network['b3']

    a1 = np.dot(x , W1) + b1
    z1 = sigmoid(a1)
    a2 = np.dot(z1, W2) + b2
    z2 = sigmoid(a2)
    a3 = np.dot(z2, W3) + b3
    y  = softmax(a3)

    return y


if __name__ == '__main__':
    x, t = get_data()
    network = init_network()

    accuracy_cnt = 0
    for i in range( len(x) ):
        y = predict(network, x[i])
        p = np.argmax(y)
        if p == t[i]:
            accuracy_cnt += 1

    print("Accuracy: %s" % (float(accuracy_cnt) / len(x)))

