#!/usr/bin/env python3

import sys, os
sys.path.append( os.path.dirname(os.path.abspath(__file__)) + '/../../dataset')
print("1/3")

from mnist import load_mnist

print("2/3")

(x_train, t_train), (x_test, t_test) = load_mnist(flatten=True, normalize=False)

print("3/#")

print(x_train.shape)
print(t_train.shape)
print(x_test.shape)
print(t_test.shape)

