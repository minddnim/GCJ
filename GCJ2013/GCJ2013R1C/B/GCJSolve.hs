import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Ratio
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [x, y] <- map read . words <$> getLine
    let (n, w) = minWork x y 0 1
    printf "Case #%d: %s\n" (i :: Int) (slv x y n w)
 
minWork :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
minWork x y n i | (abs(x) + abs(y) <= n) && ((abs(x) + abs(y)) `mod` 2) == (n `mod` 2) = (n, i-1)
                | otherwise = minWork x y (n+i) (i+1)
 
slv :: Integer -> Integer -> Integer -> Integer -> String
slv x y n w | (n /= 0) && (abs(x) >= abs(y)) && (x >= 0) = slv (x-w) y (n-w) (w-1) ++ "E"  
            | (n /= 0) && (abs(x) >= abs(y)) && (x <  0) = slv (x+w) y (n-w) (w-1) ++ "W"
            | (n /= 0) && (abs(x) <  abs(y)) && (y >= 0) = slv x (y-w) (n-w) (w-1) ++ "N"
            | (n /= 0) && (abs(x) <  abs(y)) && (y <  0) = slv x (y+w) (n-w) (w-1) ++ "S"
            | otherwise = ""