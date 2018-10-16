#!/usr/bin/env python

import numpy as np

def mean_squared_error(y, t):
    '''
    # 二乗和誤差

    ## 定義

    let
        y : Array of Real (0.0<=elm<=1.0) # ニューラルネットワークの出力
        t : Array of Real (0 or 1)        # 教師データ .. 正解が1, その他は0 (one hot 表現という)
    in
        E = 1/2 * (sum <| map λk-> (y[k] - t[k]) ^ 2)
    '''

    return 0.5 * np.sum((y-t)**2)


def cross_entropy_error(y, t):
    '''
    # 交差エントロピー誤差

    ## 定義

    let 
        y : Array of Real (0.0<=elm<=1.0) # ニューラルネットワークの出力
        t : Array of Real (0 or 1)        # 教師データ .. 正解が1, その他は0 (one hot 表現という)
    in
        E = -1 ( sum <| map λk -> t[k] * log(y[k]) )


    ## 実装上のテクニック

    log(0) を計算してしまって -∞ になるのを防ぐための微小な値 ⊿ を足しておく

    E = -1 ( sum <| map λk -> t[k] * log(y[k] + ⊿))

    '''


    delta = 1e-7
    return -np.sum(t * np.log(y + delta))


if __name__ == '__main__':
    data = [ { 'description': "「2」 の確率が最も高い場合 (0.6)",
               'y': [0.1, 0.05, 0.6, 0.0, 0.05, 0.1, 0.0, 0.1, 0.0, 0.0],
               't': [0  , 0   ,   1,   0,    0,   0,   0,   0,   0,   0]
             },
             { 'description': "「7」 の確率が最も高い場合 (0.6)",
               'y': [0.1, 0.05, 0.1, 0.0, 0.05, 0.1, 0.0, 0.6, 0.0, 0.0],
               't': [0  , 0   ,   1,   0,    0,   0,   0,   0,   0,   0]
             },
             { 'description': "「2」 の確率が100%の場合",
               'y': [0.0, 0.0 , 1.0, 0.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
               't': [0  , 0   ,   1,   0,    0,   0,   0,   0,   0,   0]
             },
             { 'description': "「7」 の確率が100%の場合",
               'y': [0.0, 0.0 , 0.0, 0.0,  0.0, 0.0, 0.0, 1.0, 0.0, 0.0],
               't': [0  , 0   ,   1,   0,    0,   0,   0,   0,   0,   0]
             }
             ]

    for d in data:
        mr = mean_squared_error(np.array(d['y']), np.array(d['t']))
        cr = cross_entropy_error(np.array(d['y']), np.array(d['t']))

        print ('')
        print( d['description'])
        print( "    二乗和誤差          : %s" % mr) 
        print( "    交差エントロピー誤差: %s" % cr) 


