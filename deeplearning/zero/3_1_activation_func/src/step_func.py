#!/usr/bin/env python3

import numpy as np
import matplotlib.pylab as plt

def step_function(x):
    y = x > 0                # numpy.array に対して bool演算すると、boolean の配列が得られる
    return y.astype(np.int)  # それをintに変換する

    #return 1 if x > 0 else 0



if __name__ == "__main__":
    x = np.arange(-5.0, 5.0, 0.1)
    y = step_function(x)

    plt.plot(x,y)
    plt.ylim(-0.1, 1.1)
    plt.show()
    
