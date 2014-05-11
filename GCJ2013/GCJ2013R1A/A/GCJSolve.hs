import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [r, t] <- map read . words <$> getLine
    let ans = slv r t
    printf "Case #%d: %d\n" (i :: Int) ans

mySqrt :: Integer -> Integer
mySqrt x = until done improve x
           where done y = (y^2 <= x) && (x < (y+1)^2)  
                 improve y = div (y + (div x y)) 2

slv :: Integer -> Integer -> Integer
slv r t = div (-2*r+1 + mySqrt((2*r-1)^2 +8*t) ) 4
