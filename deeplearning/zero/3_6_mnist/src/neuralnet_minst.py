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



import time

def measure(func):
    def wrapper(*args, **kargs):
        start_time = time.time()
        
        result = func(*args, **kargs)
        
        execution_time = time.time() - start_time
        print(f'{func.__name__}: {execution_time}')
        return result
    return wrapper


@measure
def one_shot():
    x, t = get_data()
    network = init_network()

    accuracy_cnt = 0
    for i in range( len(x) ):
        y = predict(network, x[i])
        p = np.argmax(y)
        if p == t[i]:
            accuracy_cnt += 1

    print("Accuracy (one_shot): %s" % (float(accuracy_cnt) / len(x)))

@measure
def batch( batch_size ):
    '''
    入力データを複数束ねて処理（行列演算）すると、高速化が期待できる。
    なぜならば、数値演算を行うライブラリは大きな配列の計算を効率良く行えるように最適化されていることが多いから
    '''

    x, t = get_data()
    network = init_network()

    accuracy_cnt = 0
    for i in range(1, len(x), batch_size):
        x_chank = x[i : i+batch_size]

        y = predict(network, x_chank)
        p = np.argmax(y, axis=1)
        accuracy_cnt += np.sum( p == t[i : i+batch_size])

    print("Accuracy (batch %d): %s" % (batch_size, (float(accuracy_cnt) / len(x))))


if __name__ == '__main__':
    one_shot()

    batch(100)

