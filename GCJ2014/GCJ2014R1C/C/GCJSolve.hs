import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [n, m, k] <- map read . words <$> getLine
    printf "Case #%d: %d\n" (i :: Int) $ slv n m k
 
slv :: Integer -> Integer -> Integer -> Integer
slv n m k | n <= 2 || m <= 2 = k
          | k <= 4 = k
          | n * m - k <= 4 = (n - 2) * 2 + (m - 2) * 2 + 4 - (n * m - k)
          | otherwise = 
  where rects = searchCntStone (n, m, (k + 4))

searchCntStone :: (Integer, Integer, Integer)
searchCntStone (n, m, k) w h k ret |  


