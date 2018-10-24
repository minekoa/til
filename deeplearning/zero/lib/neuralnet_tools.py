#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import numpy as np

##------------------------------------------------------------
## activation functions (活性化関数)
##------------------------------------------------------------


def sigmoid(x):
    """
    # シグモイド関数

    活性化関数として使う (別の選択肢として ステップ関数 `λx -> 1 if x > 0 else 0` があり)

    ※ステップ関数もシグモイド関数も非線形関数。
      ちなみに、  ニューラルネットワークでは活性化関数に非線型関数を使う必要がある
      なぜならば、線形関数の場合はどんなに層を厚くしても、それとおなじことを行う「隠れ層のないネットワーク」が存在する為、多層にする意味がなくなる

    ## 定義

    h(x) =     1
          -------------
          1 + exp( -x )

    """
    return 1 / (1 + np.exp(-x))

def softmax(a):
    """
    # ソフトマックス関数

    出力層の活性化関数。
    一般的に回帰問題ではid関数を、分類問題ではソフトマックス関数を使う。

    ## 定義

    let 
      a : Array of Real
      n = len(a)
    in 
        k ∈ [1 .. n] in

           y[k]  =                exp( a[k] ) 
                   ----------------------------------------
                    sum  <| map λi -> exp(a[i]) <| [1 .. n]


    ただし、指数関数 exp(x) によるオーバーフローを防ぐため
    定数項 C = max(a) を a[k] より引く

    式変形は以下

        y[k] =  exp(a[k])          / ( sum <| map λi -> exp(a[i])         <| [1..n] )
             = (exp(a[k]) * C)     / ( sum <| map λi -> exp(a[i]) * C     <| [1..n] )
             =  exp(a[k] + log(C)) / ( sum <| map λi -> exp(a[i] + log(C) <| [1..n] )
             =  exp(a[k] + C' )    / ( sum <| map λi -> exp(a[i] + C'     <| [1..n] )

    ## 特徴

    ソフトマックス関数は必ず、i ∈ 実数 ; (0 <= i <= 1)  と成る。
    また、その総和は1に成る

    そのため、ソフトマックス関数の出力を「確率」として扱うことが出来る。

    ソフトマックスでは各要素の大小関係は変わらないため、分類を行うときに省略することがある。
    （一番出力の大きなニューロンをクラス分類に使うため）
    """

    c = np.max(a)
    exp_a = np.exp(a -c)
    sum_exp_a = np.sum(exp_a)
    y = exp_a / sum_exp_a
    return y


##------------------------------------------------------------
## loss functions (損失関数)
##------------------------------------------------------------


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


##------------------------------------------------------------
## 
##------------------------------------------------------------

def numerical_gradient_1d(f, x):
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

        

def numerical_gradient(f, x):
    h = 1e-4 # 0.0001
    grad = np.zeros_like(x)
    
    it = np.nditer(x, flags=['multi_index'], op_flags=['readwrite'])
    while not it.finished:
        idx = it.multi_index
        tmp_val = x[idx]
        x[idx] = float(tmp_val) + h
        fxh1 = f(x) # f(x+h)
        
        x[idx] = tmp_val - h 
        fxh2 = f(x) # f(x-h)
        grad[idx] = (fxh1 - fxh2) / (2*h)
        
        x[idx] = tmp_val # 値を元に戻す
        it.iternext()   
        
    return grad
