#!/usr/bin/env python3

import numpy as np
import matplotlib.pylab as plt


def numerical_diff(f, x):
    '''
    df(y)|dx = lim( h->0 
                  , (f(x + h) - f(x)) / h
                  )

    を愚直に実装。

    ただし、

    * hに小さすぎる値を入れると丸め誤差 (`np.float32(1e-50)` は 0.0 になっちゃう) の影響が出る
    * hが 0 に近づけないので、前方差分では誤差が生じる

    ため、その対策を取る。
    '''

    h = 1e-4
    return (f(x+h) - f(x-h)) / (2*h)


def fun_01(x):
    return 0.01 * x**2 + 0.1*x

def fun_02(x):
    return x[0]**2 + x[1]**2

def tangent_line(f, x):
    d = numerical_diff(f, x)
    print(d)
    y = f(x) - d*x
    return lambda t: d*t + y


if __name__ == '__main__':

    def simple_plot(fun):
        x = np.arange(0.0, 20.0, 0.1)
        y = fun(x)
        dydx = numerical_diff( fun, x )

        plt.xlabel('x')
        plt.ylabel('f(x)')
        plt.plot(x, y)
        plt.plot(x, dydx)
        plt.show()

    def tangent_line_plot(fun, point):
        x = np.arange(0.0, 20.0, 0.1)
        y = fun(x)

        tf = tangent_line(fun, point)
        y2 = tf(x)

        plt.plot(x, y)
        plt.plot(x, y2)
        plt.show()


    simple_plot(fun_01)
    tangent_line_plot(fun_01, 5)
    tangent_line_plot(fun_01, 10)

    
