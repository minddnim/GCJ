import Control.Monad
import Text.Printf
import Data.Ratio
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    str <- getLine
    let (p, q) = span (/='/') str
    printf "Case #%d: %s\n" (i :: Int) $ slv (read p % read (tail q))

slv :: Ratio Integer -> String
slv n | (not . checkSolve n) 1 = "impossible"
      | otherwise = show $ slvVal n

slvVal :: Ratio Integer -> Integer
slvVal n = positivePos vals 1
  where vals = map (\x -> n - 1 % (2 ^ x)) ([1..] :: [Integer])

positivePos :: [Ratio Integer] -> Integer -> Integer
positivePos [] cnt = cnt
positivePos (x:xs) cnt | x >= 0 = cnt
                       | otherwise = positivePos xs (cnt + 1)

checkSolve :: Ratio Integer -> Integer -> Bool
checkSolve n val | deno `div` val == 1 && deno `mod` val == 0 = True
                 | deno `div` val == 1 && deno `mod` val /= 0 = False
                 | otherwise = checkSolve n (val * 2)
  where deno = denominator n
