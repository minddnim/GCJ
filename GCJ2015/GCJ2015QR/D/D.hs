import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [x, r, c] <- map read . words <$> getLine
    let ans = slv x r c
    printf "Case #%d: %s\n" (i :: Int) ans

slv :: Int -> Int -> Int -> String
slv x r c | (r*c) `mod` x == 0 && x < 7 = detailCheck x r c
          | otherwise = "RICHARD"

detailCheck :: Int -> Int -> Int -> String
detailCheck x r c | x == 1 = "GABRIEL"
                  | x == 2 = "GABRIEL"
                  | x == 3 && r >= 2 && c >= 2 = "GABRIEL"
                  | x == 4 && r >= 3 && c >= 3 = "GABRIEL"
                  | x == 5 && (r >= 4 && c >= 4 || min r c == 3 && max r c >= 10)= "GABRIEL"
                  | x == 6 && r >= 4 && c >= 4 = "GABRIEL"
                  | otherwise = "RICHARD"