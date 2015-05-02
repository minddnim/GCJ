import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [r, c, m] <- map read . words <$> getLine
    printf "Case #%d:\n%s" (i :: Int) $ slv (r, c) m

slv :: (Int, Int) -> Int -> String
slv (fRow, fCol) m | m == 0 = createField (fRow, fCol) [(fRow, fCol)]
                   | blankNum == 1 = createField (fRow, fCol) [(1, 1), (fRow - 1, 0)]
                   | fRow == 1 = createField (fRow, fCol) [(1, blankNum)]
                   | fCol == 1 = createField (fRow, fCol) [(blankNum, 1), (fRow - blankNum, 0)]
                   | null blankFieldSizes || ((fRow == 2 || fCol == 2) && m `mod` 2 /= 0) = "Impossible\n"
                   | otherwise = createField (fRow, fCol) blankFieldSizes
  where blankNum = fRow * fCol - m
        blankFieldSizes = blankFieldSlv (fRow, fCol) fCol blankNum

blankFieldSlv :: (Int, Int) -> Int -> Int -> [(Int, Int)]
blankFieldSlv fSize@(fRow, fCol) col bn = blankFieldSlv' [] fSize newRow col bn []
  where newRow = rowSlv fRow col bn

blankFieldSlv' :: [((Int, Int), (Int, Int), Int)] -> (Int, Int) -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
blankFieldSlv' [] _ _ 1 _ ret = ret
blankFieldSlv' stack _ _ 1 _ ret = blankFieldSlv' (tail stack) (fRow', fCol') (row' - 1) col' bn' (tail ret)
  where ((fRow', fCol'), (row', col'), bn') = head stack
blankFieldSlv' stack fSize@(fRow, fCol) row col bn ret |　remBlank == 0 && rectCond = reverse $ (fRow - row, 0) : (row, col) : ret
                                                       | remBlank /= 1 && rectCond = blankFieldSlv' ((fSize, (row, col), bn):stack) (fRow - row, col - 1) newRow (col - 1) remBlank ((row, col):ret)
                                                       | row <= 0 = blankFieldSlv' stack fSize newRow (col - 1) bn ret
                                                       | otherwise = blankFieldSlv' stack fSize (row - 1) col bn ret
  where remBlank = bn - row * col
        rectCond = rectSizeSlvCond stack fSize row col
        newRow = rowSlv fRow (col - 1) bn

rowSlv :: Int -> Int -> Int -> Int
rowSlv fRow col bn = min fRow (bn `div` col) 

rectSizeSlvCond :: [((Int, Int), (Int, Int), Int)] -> (Int, Int) -> Int -> Int -> Bool
rectSizeSlvCond stack (fRow, fCol) row col = isRowRange && isColRange
  where isColRange = 2 <= col && col <= fCol
        isRowRange = (2 <= row && row <= fRow) || (fRow == 1 && row == 1) || (2 <= col && row == 1 && not (null stack))

remBlankSlv :: Int -> Int -> Int -> Int
remBlankSlv fRow col bn = bn - row * col
  where row = rowSlv fRow col bn

createField :: (Int, Int) -> [(Int, Int)] -> String
createField fSize bSize = 'c' : tail fieldStr
 where fieldStr = unlines $ concatMap (createField' fSize) bSize

createField' :: (Int, Int) -> (Int, Int) -> [String]
createField' (_, fCol) (r, c) = replicate r blankLine
  where blankLine = replicate c '.' ++ replicate (fCol - c) '*'
