import Control.Monad
import Text.Printf
import Control.Applicative
import qualified Data.List as L

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [_, n] <- words <$> getLine
    m <- map read.words <$> getLine
    let ans = slv (read n :: Integer) m
    printf "Case #%d: %d\n" (i :: Int) ans

slv :: Integer -> [Integer] -> Int
slv n m | remHumanIdx < 0 = fromIntegral n
        | otherwise = 1 + ((L.elemIndices True okBarbers) !! remHumanIdx)
  where prevTime = bSearch n m (0, 10^16)
        fixHumanNum = fromTime2human prevTime m
        remHumanIdx = fromIntegral (n - fixHumanNum) - 1
        okBarbers = map (\x -> (prevTime + 1) `mod` x == 0) m

bSearch :: Integer -> [Integer] -> (Integer, Integer) -> Integer
bSearch n m (start, end) | middle == start = start
                         | middleCnt >= n = bSearch n m (start, middle)
                         | middleCnt < n = bSearch n m (middle, end)
  where middle = (start + end) `div` 2
        middleCnt = fromTime2human middle m

fromTime2human :: Integer -> [Integer] -> Integer
fromTime2human t = sum . (map (\x -> (t `div` x) + 1))
