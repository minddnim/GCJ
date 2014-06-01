import Control.Monad
import Control.Applicative
import Data.Array
 
main :: IO()
main = do
  testCase <- readLn
  let bT = badAllTbl 1000 (initTbl 1000)
  print $ show bT -- ここで強制的に評価しないとout of memoryになる。
  forM_ [1..testCase] $ \i -> do
    n <- read <$> getLine
    vals <- map read . words <$> getLine
    appendFile "Ans.txt" $ "Case #" ++ show (i :: Int) ++ ": " ++slv n (zip [0..] vals) bT
 
badAllTbl :: Int -> [Array Int Double] -> Array Int (Array Int Double)
badAllTbl n it = array (0, n-1) $ zip [0..] $ map (\xs -> badTbl n xs 0 0 []) it
 
initTbl :: Int -> [Array Int Double]
initTbl n = map (array (0, n-1)) zipAllPtrList
  where zipAllPtrList = map (zip [0..]) allPtrList
        allPtrList = makeLists n (makeList n) []
 
makeLists :: Int -> [Double] -> [[Double]] -> [[Double]]
makeLists _ [] ret = reverse ret
makeLists n ls ret = makeLists n xs (x:ret)
  where (x, xs) = splitAt n ls 
 
makeList :: Int -> [Double]
makeList n = take (n*n) $ cycle ptn
  where ptn = 1.0 : replicate n 0.0
 
slv :: Int -> [(Int, Int)] -> Array Int (Array Int Double) -> String
slv n vals bT | isGood n vals bT 1.0 = "GOOD"
              | otherwise = "BAD"
 
isGood :: Int -> [(Int, Int)] -> Array Int (Array Int Double) -> Double -> Bool
isGood _ [] _ ans = ans <= 1.0
isGood n ((i, v):vs) bT ans = isGood n vs bT (ans * realToFrac n * (bT!v)!i) 
 
badTbl :: Int -> Array Int Double -> Int -> Int -> [Double] -> Array Int Double
badTbl n ps i k vs | n == i = ps
                   | n == k = badTbl n cArr (i+1) 0 []
                   | otherwise = badTbl n ps i (k+1) (ptn:vs)
  where cArr = array (0, n-1) $ zip [0..] (reverse vs)
        ptn | i == k = 1.0 / realToFrac n
            | otherwise = (ps!i + (ps!k)*(realToFrac n-1)) / realToFrac n