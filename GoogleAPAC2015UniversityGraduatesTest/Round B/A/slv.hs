import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [m, n] <- map read.words <$> getLine
    let ans = slv m n
    printf "Case #%d: %d\n" (i :: Int) ans

slv :: Integer -> Integer -> Integer
slv m n = ans `mod` (10^9+7)
  where ans = sum $ map (sign) $ zip (reverse(map (\x -> comb m x * x^n) [1..m])) [0..]
        sign (x, n) = x * (-1)^n

fact :: Integer -> Integer
fact n = product [1..n]

comb :: Integer -> Integer -> Integer
comb n k = (fact n) `div` ((fact k) * (fact (n-k)))