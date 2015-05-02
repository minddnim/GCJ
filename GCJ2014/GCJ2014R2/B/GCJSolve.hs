import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    _ <- getLine
    a <- map read . words <$> getLine
    printf "Case #%d: %d\n" (i :: Int) $ slv a 0

slv :: [Integer] -> Integer -> Integer
slv [] ret = ret
slv xs ret = slv (delete minX xs) (ret + fromIntegral swapCnt)
  where minX = minimum xs
        swapCnt = min swapLeftCnt swapRightCnt
        swapLeftCnt = fromIntegral $ head $ elemIndices minX xs
        swapRightCnt = fromIntegral $ (length xs - 1) - swapLeftCnt