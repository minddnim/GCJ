import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    n <- read <$> getLine
    ws <- replicateM n $ ((\[x,y] -> (x,y) :: (Int, Int)) . map read . words) <$> getLine
    let ans = length [ b | c <- ws, b <- ws, fst b < fst c, snd b > snd c ]
    printf "Case #%d: %d\n" (i :: Int) ans
