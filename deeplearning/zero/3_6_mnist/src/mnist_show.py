#!/usr/bin/env python3
import sys, os

sys.path.append( os.path.dirname(os.path.abspath(__file__)) + '/../../dataset')
from mnist import load_mnist


import numpy as np
from PIL import Image


def img_show( img ):
    pil_img = Image.fromarray(np.uint8(img))
    pil_img.show()


if __name__ == '__main__':
    (x_train, t_train), (x_test, t_test) = load_mnist(flatten=True, normalize=False)

    img = x_train[0]
    label = t_train[0]
    print( "label              =%s" % label)
    print( "img.shape          =%s" % img.shape)

    img2d = img.reshape(28,28)
    print( "img.reshape(28,28) =\n%s" % img2d)
    
    img_show(img2d)
    
