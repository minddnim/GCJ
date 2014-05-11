import Control.Monad
import Text.Printf
import Control.Applicative
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [c, f, x] <- map read . words <$> getLine
    printf "Case #%d: %.7f\n" (i :: Int) $ slv 2.0 c f x
 
slv :: Double -> Double -> Double -> Double -> Double
slv s c f x = slvMinTime s c f x 1.0 0.0 (x/s)

slvMinTime :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
slvMinTime s c f x n sumT nowT | nowT < baseT = min tmpT nowT
                               | otherwise = slvMinTime s c f x (n+1.0) baseT (min tmpT nowT)
  where baseT = c/(s+f*(n-1.0)) + sumT
        tmpT = x/(s+f*n) + baseT