import Control.Monad
import Text.Printf
import Control.Applicative

getPt :: IO (Int, Int)
getPt = do
  [x, y] <- map read . words <$> getLine
  return (x, y)

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    n <- read <$> getLine
    pt <- replicateM n $ getPt
    let ans = slv n pt
    printf "Case #%d:\n%s" (i :: Int) ans

slv :: Int -> [(Int, Int)] -> String
slv n pts = unlines $ map show logIntList
  where logIntList | n <= 3 = replicate n 0
                   | otherwise = slvIntList pts pts

slvIntList :: [(Int, Int)] -> [(Int, Int)] -> [Int]
slvIntList basePt1s basePt2s = go basePt2s basePt1s basePt2s [] []
  where go tmp pts@(x:xs) (y:ys) ret ans | x == y = go tmp pts ys ret ans
                                         | otherwise = go tmp pts ys ((check x y tmp (0, 0)):ret) ans
        go tmp (x:xs) [] ret ans = go tmp xs tmp [] ((minimum ret):ans)
        go _ [] _ ret ans = reverse ans

check :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int
check p@(x1, y1) q@(x2, y2) ((x, y):pts) (l, r) | (y2-y1)*x + (x1-x2)*y + (x2*y1-x1*y2) > 0 = check p q pts (l+1, r)
                                                | (y2-y1)*x + (x1-x2)*y + (x2*y1-x1*y2) < 0 = check p q pts (l, r+1)
                                                | otherwise = check p q pts (l, r)
check _ _ [] (l, r) = min l r
