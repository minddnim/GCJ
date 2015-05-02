import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    m <- map read.words <$> getLine
    printf "Case #%d: %d\n" (i :: Int) $ slv m

slv :: [Integer] -> Integer
slv m = undefined