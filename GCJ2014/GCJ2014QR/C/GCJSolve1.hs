import Control.Monad
import Control.Monad.State
import Text.Printf
import Control.Applicative
import Data.List

type FieldSize = (Int, Int)
type RectSize = (Int, Int)
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [r, c, m] <- map read . words <$> getLine
    printf "Case #%d:\n%s" (i :: Int) $ slv (r, c) m

slv :: FieldSize -> Int -> String
slv f@(fx, fy) m | m == 0 = ansFld f [f]
                 | blankNum == 1 = ansFld f [(1, 1)]
                 | fx == 1 = ansFld f [(1, blankNum)]
                 | fy == 1 = ansFld f [(blankNum, 1)]
                 | null blankFldSizes = "Impossible\n"
                 | otherwise = ansFld f blankFldSizes
  where blankNum = fx * fy - m
        blankFldSizes = blankFldSlv f blankNum

blankFldSlv :: FieldSize -> Int -> [RectSize]
blankFldSlv f bn = execState $ fldSlv f bn (state [] ((), []))

fldSlv :: FieldSize -> Int -> State [RectSize] () -> State [RectSize] ()
fldSlv f bn s = undefined

ansFld :: FieldSize -> [RectSize] -> String
ansFld f@(_, fy) rs = "c" ++ tail creFld
  where creFld = foldl (\f' r' -> f' ++ creBlockFld f r') [] rs ++ mineFld
        mineFld = creBlockFld f (0, fy - sum (map snd rs))

creBlockFld :: FieldSize -> RectSize -> String
creBlockFld (fx, _) (rx, ry) = unlines $ replicate ry creFldLine
  where creFldLine = replicate rx '.' ++ replicate (fx - rx) '*'
