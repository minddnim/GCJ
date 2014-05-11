import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Bits

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [a, b, k] <- map read . words <$> getLine
    printf "Case #%d: %d\n" (i :: Integer) $ slv (a, b, k)

slv :: (Integer, Integer, Integer) -> Integer
slv (a, b, k) | b < a = slv' (b, a, k) (b-1) a 0
              | otherwise = slv' (a, b, k) (a-1) b 0

slv' :: (Integer, Integer, Integer) -> Integer -> Integer -> Integer -> Integer
slv' (a, b, k) i j ret | i < k = ret + (i+1)*b
                       | otherwise = slv' (a, b, k) (i-1) b (ret+cnt)
  where cnt = cntVal i (j-1) k 0

cntVal :: Integer -> Integer -> Integer -> Integer -> Integer
cntVal i j k ret | j == 0 = ret + 1
                 | otherwise = cntVal i (j-1) k (ret+val)
  where val | i .&. j < k = 1
            | otherwise = 0
