import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [n, dir] <- words <$> getLine
    mat <- replicateM (read n :: Int) $ map read . words <$> getLine
    let ans = slv (read n :: Int, dir, mat)
    printf "Case #%d:\n%s" (i :: Int) (outputMat ans)

outputMat :: [[Integer]] -> String
outputMat mat = unlines $ map (concat . intersperse " " . map show) mat

slv :: (Int, String, [[Integer]]) -> [[Integer]]
slv (n, "left", mat) = dirLeft n mat
slv (n, "right", mat) = dirRight n mat
slv (n, "up", mat) = dirUp n mat
slv (n, "down", mat) = dirDown n mat
slv input = error (show input)

dirLeft :: Int -> [[Integer]] -> [[Integer]]
dirLeft n mat = map (moveLeft n) mat

dirRight :: Int -> [[Integer]] -> [[Integer]]
dirRight n mat = map (reverse . (moveLeft n) . reverse) mat

dirUp :: Int -> [[Integer]] -> [[Integer]]
dirUp n mat = transpose $ dirLeft n (transpose mat)

dirDown :: Int -> [[Integer]] -> [[Integer]]
dirDown n mat = transpose $ dirRight n (transpose mat)

moveLeft :: Int -> [Integer] -> [Integer]
moveLeft n row = move row 0 []
  where move [] tmp ret = reverse (replicate (n- (length (tmp:ret))) 0 ++ (tmp:ret))
        move (x:xs) 0 ret = move xs x ret
        move (x:xs) tmp ret | x == 0 = move xs tmp ret
                            | x == tmp = move xs 0 ((tmp*2):ret)
                            | otherwise = move xs x (tmp:ret)
