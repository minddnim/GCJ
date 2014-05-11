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
          | otherwise = searchCntStone [(w, h)| w <- [1..n], h <- [1..m]] k

searchCntStone :: [(Integer, Integer)] -> Integer -> Integer
searchCntStone rects k = minimum $ concatMap (cntStone k) rects 

cntStone :: Integer -> (Integer, Integer) -> [Integer]
cntStone k (w, h) | space > k = []
                  | otherwise = [2 * w + 2 * h + r]
  where space = 2 * w + h * (w + 2)
        r = k - space
