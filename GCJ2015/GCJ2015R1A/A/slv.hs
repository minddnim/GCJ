import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    _ <- readLn
    m <- map read.words <$> getLine
    let ans = slv m
    printf "Case #%d: %d\n" (i :: Int) ans

slv :: Integer -> (Integer, Integer)
slv m = (fWay m, sWay m)

fWay :: Integer -> Integer
fWay m = undefined

sWay :: Integer -> Integer
sWay m = undefined

