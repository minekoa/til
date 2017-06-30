import Debug.Trace
import Control.Monad


sqrt' :: Float -> Float
sqrt' x = until goodEnough improove 1
  where  
--1.    goodEnough y = abs (y ^^ (2 :: Int) -x) < epsilon * x
--2.    goodEnough y = abs (y ^^ (2 :: Int) -x) <= epsilon * x
    goodEnough y = abs (({-join traceShow-} y) ^^ (2 :: Int) -x) <= 2 * epsilon1 * x -- それはそれとして、近づいているかをチェックするときに2じょう しているので、2*epsilon
    improove y  = (y + x / y) / 2
--    epsilon     = 10 ^^ (-6 :: Int)
    epsilon1     = 2 * until (\x -> 1 + x == 1) (/ 2) 1  -- マシンイプシロンは足しても増えないので、ほんのちょっとだけ大きくするので * 2
                                                            


-- マシンイプシロンを求める
-- 求めたら < じゃ非停止
-- epsilon を 2倍する

-- sqrt' 6 で停止しない
-- デバッグする traceShow と joinを使う

