import Control.Monad
import Text.Printf
import Control.Applicative

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [_, a] <- words <$> getLine
    let ans = slv (zip a [1..]) (0, 0)
    printf "Case #%d: %d\n" (i :: Int) ans

slv :: [(Char, Integer)] -> (Integer, Integer) -> Integer
slv [] (nowTotal, needTotal) = needTotal
slv ((a, i):as) (nowTotal, needTotal) | i > (ai + nowTotal) = slv as (i, needTotal + (i-(ai + nowTotal)))
                                      | otherwise = slv as (nowTotal + ai, needTotal)
  where ai = read [a] :: Integer

