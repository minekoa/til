#!/usr/bin/env python

import numpy as np

a = np.array([0.3, 2.9, 4.0])
exp_a = np.exp(a)
sum_exp_a = np.sum(exp_a)
y = exp_a / sum_exp_a

def softmax(a):
    exp_a = np.exp(a)
    sum_exp_a = np.sum(exp_a)
    y = exp_a / sum_exp_a
    return y


print ("SOFT MAX")
print ("")
print ( "                     exp( a[k] )")
print ( "y[k] = ---------------------------------------")
print ("          Σ ( range(1..n), λi -> exp( a[i] ) )")
print ("")
print ( "a        = %s" % a)
print ( "exp_a    = %s" % exp_a)
print ( "sum_exp_a= %s" % sum_exp_a)
print ( "y        = %s" % y)
