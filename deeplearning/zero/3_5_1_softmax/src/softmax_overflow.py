#!/usr/bin/env python3

import numpy as np

print( """
# ソフトマックス関数は、指数関数の演算 exp(a[i]) を行うため、
# オーバーフローが起きやすい (例えば e^1000 はinf が帰ってくるだろう)
#
# そこで、以下のように式を変形する
#
#  y[k] = exp( a[k] )          / sum(1..n, λi -> exp( a[i] ) )
#       = exp( a[k] ) * C      / sum(1..n, λi -> exp( a[i] ) )  * C
#       = exp( a[k] + log(C) ) / sum(1..n, λi -> exp( a[i] + log(C) ) ) 
#       = exp( a[k] + C' )     / sum(1..n, λi -> exp( a[i] + C' ) )
#
# つまり、exp に渡す数に任意の定数を足したり引いたりしても結果は変わらない
#
# これを利用して、入力信号の最大値を碾いてあげることでオーバーフローを防ぐ
""")

print ("\n==== PURE SOFTMAX ====")
a = np.array( [1010, 100, 900] )
exp_a = np.exp(a)
y = exp_a / np.sum(exp_a)

print( "a     = %s"  % a)
print( "exp(a)= %s"  % exp_a)
print( "y     = %s" % y)

# exp_a = [           inf 2.68811714e+43            inf]
# y     = [nan  0. nan]



print("\n==== Anti OVERFLOW SOFTMAX ====")
c = np.max(a)
a2 = a - c
exp_a2 = np.exp(a2)
y2 = exp_a2 / np.sum(exp_a2)

print ( "a = %s, c = %s" % (a, c) )
print ( "a-c        = %s"  % a2)
print ( "exp( a-c ) = %s"  % exp_a2)
print ( "y          = %s"  % y2)


print ("""
# ソフトマックス関数の特徴
#
# ソフトマックス関数は必ず、i ∈ 実数 ; (0 <= i <= 1)  と成る。
# また、その総和は1に成る

そのため、ソフトマックス関数の出力を「確率」として扱うことが出来る。
""")

print ( "sum(y) = %s" % np.sum(y2) )

