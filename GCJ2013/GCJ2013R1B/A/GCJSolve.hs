import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [a, n] <- map read . words <$> getLine
    motes <- map read . words <$> getLine
    let motesSort = sort motes
    printf "Case #%d: %d\n" (i :: Int) (slv a motesSort 0 [] (length motesSort))
 
slv :: Int -> [Int] -> Int -> [Int] -> Int -> Int
slv a [] result results bakcount | length results /= 0 = min result (minimum results)
                                 | otherwise = result
slv a (x:xs) result results bakcount | x < a = slv (a+x) xs result (results ++ [result + length xs]) bakcount
                                     | x >= a && bakcount > res1 = slv (a + (a-1)) (x:xs) res1 (results ++ [res1 + length (x:xs)]) bakcount
                                     | x >= a && bakcount <= res1 = slv (a + (a-1)) [] bakcount results bakcount
                                     where res1 = result + 1