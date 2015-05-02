import Control.Monad
import Text.Printf
import Control.Applicative
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO()
main = do
  testCase <- readLn
  let tbl = makeTbl 1000 [0..999]
  forM_ [1..testCase] $ \i -> do
    [a, b, c] <- map read . words <$> getLine
    printf "Case #%d: %d\n" (i :: Int) $ slv tbl a b c

makeTbl :: Int -> [Int] -> V.Vector (V.Vector (Int, Int))
makeTbl n base = makeTbl' n base []
  where makeTbl' 0 base ret = V.fromList ret
        makeTbl' n base ret = makeTbl' (n-1) base ((V.fromList (map (\x -> getPQ x n) base)):ret)

slv :: V.Vector (V.Vector (Int, Int)) -> Int -> Int -> Int -> Int
slv tbl a 0 m = a `mod` m
slv tbl a n m = (slv tbl a (n-1) m)^index `mod` m
  where (q, p) = (tbl V.! (n-1)) V.! m
        index | n < q = n
              | otherwise = q + (slv tbl a (n-1) p)

getPQ :: Int -> Int -> (Int, Int)
getPQ base inMod = getPQ' base inMod 1 M.empty
  where getPQ' b m n remMap = case M.lookup b remMap of
                              Just a -> (a, n-(a-1))
                              Nothing -> getPQ' (b*b `mod` m) m (n+1) (M.insert (b `mod` m) n remMap)
