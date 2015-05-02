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
slv m n = undefined