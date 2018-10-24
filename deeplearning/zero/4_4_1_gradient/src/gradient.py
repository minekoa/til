#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import numpy as np
import matplotlib.pylab as plt

def numerical_gradient(f, x):
    '''
    # 数値勾配

    ## 定義

    すべての変数の偏微分をまとめたものを勾配(gradient)という。

    '''


    h = 1e-4
    grad = np.zeros_like(x) # xと同じshapeで要素がall0の配列を生成

    for idx in range(x.size):
        tmp_val = x[idx] # 退避

        x[idx] = tmp_val + h
        fxh1 = f(x)

        x[idx] = tmp_val - h
        fxh2 = f(x)

        grad[idx] = (fxh1 - fxh2) / (2 * h)

        x[idx] = tmp_val # 復帰

    return grad



def gradient_descent(f, init_x, lr=0.01, step_num=100):
    '''
    勾配降下法
    '''

    x = init_x

    proc = [] # 勾配を下る姿を記録（グラフに出したいので）
    proc.append([x[0], x[1]])

    grads = []

    for i in range(step_num):
        grad = numerical_gradient(f, x)
        x -= lr * grad

        proc.append([x[0], x[1]])
        grads.append(grad)


    return (x, proc, grads)



def fun(x):
    return (x[0] ** 2) + (x[1] ** 2)
        



if __name__ == '__main__':


    # 勾配の確認

    print ("gradient (3,4)= %s " % numerical_gradient(fun, np.array([3.0, 4.0])))
    print ("gradient (0,2)= %s " % numerical_gradient(fun, np.array([0.0, 2.0])))
    print ("gradient (3,0)= %s " % numerical_gradient(fun, np.array([3.0, 0.0])))


    # 勾配の表示
    xx, yy = np.meshgrid( np.arange(-4, 4, 0.5),
                          np.arange(-5, 5, 0.5) )

    for x, y in zip (xx, yy):
        for xi, yi in zip (x, y):
            g = numerical_gradient(fun, np.array([xi, yi]))
            plt.quiver(xi, yi,
                       g[0], g[1],
                       color='silver',
                       angles='xy',
                       scale_units='xy',
                       scale=10
            )
        



    # 勾配降下法の確認
    init_x = np.array([-3.0, 4.0])

    ans , proc, grads  = gradient_descent(fun,
                                          init_x = init_x,
                                          lr=0.1, #1e-10, #10,
                                          step_num=100)

    print( "ans: %s" % ans)
    
    plt.xlabel('x[0]')
    plt.ylabel('x[1]')
    plt.xlim(-4, 4)
    plt.ylim(-5, 5)

    plt.plot([x[0] for x in proc], [x[1] for x in proc], marker='o')

    # for i in range( len(grads) ):
    #     plt.quiver(proc[i][0], proc[i][1],    # 始点 (x,y)
    #                grads[i][0], grads[i][1],  # 成分 (u,v)
    #                color='gray',
    #                angles='xy',
    #                scale_units='xy',
    #                scale=10
    #                )


    plt.show()


    
