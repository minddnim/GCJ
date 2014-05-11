import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    sr <- readLn
    r1 <- map read . words <$> getLine
    r2 <- map read . words <$> getLine
    r3 <- map read . words <$> getLine
    r4 <- map read . words <$> getLine
    sr' <- readLn
    r1' <- map read . words <$> getLine
    r2' <- map read . words <$> getLine
    r3' <- map read . words <$> getLine
    r4' <- map read . words <$> getLine
    let sr1 = sr :: Int
        sr2 = sr' :: Int
        grid = [r1, r2, r3, r4]
        grid' = [r1', r2', r3', r4']
    printf "Case #%d: %s\n" (i :: Int) $ slv (sr1-1) grid (sr2-1) grid'
 
slv :: Int -> [[Int]] -> Int -> [[Int]] -> String
slv select grid select' grid' | null intersectCard = "Volunteer cheated!"
                              | length intersectCard == 1 = show $ head intersectCard
                              | otherwise = "Bad magician!"
  where selectRow = grid !! select
        selectRow' = grid' !! select'
        intersectCard = selectRow `intersect` selectRow'


