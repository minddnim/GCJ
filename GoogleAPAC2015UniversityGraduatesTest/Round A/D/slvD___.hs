import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Maybe
import Data.List

-- main :: IO()
-- main = do
--   testCase <- readLn
--   forM_ [1..testCase] $ \i -> do
--     (_:m:tiles) <- map read . words <$> getLine
--     let ans = slv m tiles
--     printf "Case #%d: %d\n" (i :: Int) ans

-- slv :: Integer -> [Integer] -> Integer
-- slv m tiles = needTile m [(m, m)] ((reverse . sort) [2^x | x<-tiles]) 1

-- putTile :: Integer -> [(Integer, Integer)] -> Integer -> (Integer, [(Integer, Integer)])
-- putTile m tiles size | isNothing putTileResult = (1, cutTile size (m, m) ++ tiles)
--                      | otherwise = (0, cutT ++ remT)
--   where putTileResult = minTile $ filter (checkPutTile size) tiles
--         (minT, remT) = fromJust putTileResult
--         cutT = cutTile size minT

-- needTile :: Integer -> [(Integer, Integer)] -> [Integer] -> Integer -> Integer
-- needTile _ _ [] cnt = cnt
-- needTile m tileMs (n:ns) cnt = needTile m remTileSizes ns (cnt + newTileCnt)
--   where (newTileCnt, remTileSizes) = putTile m tileMs n

checkPutTile :: Integer -> (Integer, Integer) -> Bool
checkPutTile size (w, h) = w >= size && h >= size

minTile :: [(Integer, Integer)] -> Maybe ((Integer, Integer), [(Integer, Integer)])
minTile [] = Nothing
minTile (t:ts) = Just (minTile' ts (t, []))
  where minTile' [] ret = ret
        minTile' (x@(tw, th):xs) (ret@(rw, rh), remTiles) | (tw * th) < (rw * rh) = minTile' xs ((tw, th), ret:remTiles)
                                                          | otherwise = minTile' xs (ret, x:remTiles)

cutTile :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
cutTile size tile = filter (\(w, h) -> w * h /= 0) $ cutTile' size tile
  where cutTile' s (w, h) | w >= h = [(w - s, h), (s, h - s)]
                          | otherwise = [(w - s, s),(w, h - s)]

slv :: Integer -> [Integer] -> (Integer, String)
slv m tiles = needTile m [(m, m)] ((reverse . sort) [2^x | x<-tiles]) (1, "")

putTile :: Integer -> [(Integer, Integer)] -> Integer -> (Integer, [(Integer, Integer)], String)
putTile m tiles size | isNothing putTileResult = (1, cutTile size (m, m) ++ tiles, "Nothing " ++ show (m, tiles, size) ++ "\n")
                     | otherwise = (0, cutT ++ remT, "Just putTile: " ++ show (m, tiles, size, cutT, remT) ++ "\n")
  where putTileResult = minTile $ filter (checkPutTile size) tiles
        (minT, remT) = fromJust putTileResult
        cutT = cutTile size minT

needTile :: Integer -> [(Integer, Integer)] -> [Integer] -> (Integer, String) -> (Integer, String)
needTile _ _ [] cnt = cnt
needTile m tileMs (n:ns) (cnt, lt) = needTile m remTileSizes ns ((cnt + newTileCnt), lt ++ logTxt)
  where (newTileCnt, remTileSizes, logTxt) = putTile m tileMs n
