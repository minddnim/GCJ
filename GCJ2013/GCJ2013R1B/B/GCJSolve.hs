import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Ratio

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [n, x, y] <- map read . words <$> getLine
    let absX = abs(x)
    printf "Case #%d: %f\n" (i :: Int) (slv n absX y)

slv :: Integer -> Integer -> Integer -> Double
slv n x y | n == 0 = 0.0
          | x == 0 = funcY n y
          | n < lBound x y = 0.0
          | lBound x y <= n && n < rBound x y = fromRational(func (n - (lBound x y) + 1) x y)
          | rBound x y <= n = 1.0

lBound :: Integer -> Integer -> Integer
lBound x y = 2*(val x y)^2 - (val x y) + 1

rBound :: Integer -> Integer -> Integer
rBound x y = ((val x y) +1)*(2*(val x y) + 1)

func :: Integer -> Integer -> Integer -> Rational
func n x y = (pattern n (x+y) (max (y+1) (n-(x+y)))) % (pattern n (x+y) (max (n-(x+y)) 0) )

funcY :: Integer -> Integer -> Double
funcY n y | n < rBound 0 y = 0.0
          | otherwise = 1.0

pattern :: Integer -> Integer -> Integer -> Integer
pattern n k b | k < b = 0
              | otherwise = (comb n k) + pattern n (k-1) b

val :: Integer -> Integer -> Integer
val x y = div (x+y) 2

comb :: Integer -> Integer -> Integer
comb n k | n < k = 0
         | otherwise = div (fact n) (fact (n-k) * fact k) 

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)
