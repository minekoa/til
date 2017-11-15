{-# LANGUAGE TemplateHaskell, Rank2Types, ViewPatterns #-}
import Control.Lens

sample :: IO ()
sample = do
  print $ (1,2) ^. _1               -- 1
  print $ (1,2) & _1 .~ 3           -- (3,2)
  print $ ((1,2),3) ^. _1 . _1      -- 1
  print $ ((1,2),3) & _1 . _2 .~ 3  -- ((3,2),3)

data XY a = XY {
   _x :: a
 , _y :: a
} deriving (Show, Eq)

makeLenses ''XY

-- getYY (XY (1, 2) (3, 4)) == 4
getYY :: XY (Int, Int) -> Int
getYY xy =
  xy ^.y^._2
--  xy ^.y._2        -- カッコはいらない
--  xy ^.(y._2)      -- 関数合成
--  (xy ^. y) ^. _2  -- 素朴に


-- updateYY (XY (1, 2) (3, 4)) succ == (XY (1, 2) (3, 5))
updateYY :: XY (Int, Int) -> (Int -> Int) -> XY (Int, Int)
updateYY = undefined

-- get2dList 0 1 [[2,3],[4,5]] == Just 3
-- get2dList 0 2 [[2,3],[4,5]] == Nothing
get2dList :: Int -> Int -> [[Int]] -> Maybe Int
get2dList = undefined

-- update2dList 0 1 [[2,3],[4,5]] succ == Just 4
-- update2dList 0 2 [[2,3],[4,5]] succ == Nothing
update2dList :: Int -> Int -> [[Int]] -> (Int -> Int) -> Maybe [[Int]]
update2dList = undefined

-- updateListOnlyOdd [1,2,3,4] succ == [1,3,3,5]
updateListOnlyOdd :: [Int] -> (Int -> Int) -> [Int]
updateListOnlyOdd = undefined

check :: [Bool] -> IO ()
check (all id -> True) = putStrLn "SUCCESS"
check _ = putStrLn "FAIL"

test :: IO ()
test = do
    check []
    check [getYY (XY (1, 2) (3, 4)) == 4]
    check [updateYY (XY (1, 2) (3, 4)) succ == XY (1, 2) (3, 5)]
    check [ get2dList 0 1 [[2,3],[4,5]] == Just 3
          , get2dList 0 2 [[2,3],[4,5]] == Nothing]
    check [ update2dList 0 1 [[2,3],[4,5]] succ == Just [[2,4],[4,5]]
          , update2dList 0 2 [[2,3],[4,5]] succ == Nothing]
    check [ updateListOnlyOdd [1,2,3,4] succ == [1,3,3,5]]

main = do
    sample
    --check []
    test
