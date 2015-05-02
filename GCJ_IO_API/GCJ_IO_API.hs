-- problem ----------------------------------------------
-- 2
-- 1 9
-- 1 10

-- import Control.Monad
-- import Text.Printf
-- import Control.Applicative

-- main :: IO()
-- main = do
--   testCase <- readLn
--   forM_ [1..testCase] $ \i -> do
--     [r, t] <- map read . words <$> getLine
--     let ans = slv r t
--     printf "Case #%d: %d\n" (i :: Int) ans



-- problem ----------------------------------------------

-- 1
-- 2
-- 1 2 3 4
-- 5 6 7 8
-- 9 10 11 12
-- 13 14 15 16
-- 3
-- 1 2 5 4
-- 3 11 6 15
-- 9 10 7 12
-- 13 14 8 16

-- import Control.Monad
-- import Text.Printf
-- import Control.Applicative

-- getInts :: IO [Int]
-- getInts = map read . words <$> getLine

-- main :: IO()
-- main = do
--   testCase <- readLn
--   forM_ [1..testCase] $ \i -> do
--     r1 <- readLn
--     b1 <- replicateM 4 $ getInts
--     r2 <- readLn
--     b2 <- replicateM 4 $ getInts

------------------------------------------------------------
-- 二進数の文字列から10進数の整数型に変換
-- s2b :: String -> Integer
-- s2b str = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) str

-- 10進数の整数型から二進数の文字列に変換
-- b2s ::Integer -> String
-- b2s v = b2s' v ""
--   where b2s' 0 ret = ret
--         b2s' val ret = b2s' (val `div` 2) (modChar val:ret)
--         modChar n | n `mod` 2 == 1 = '1'
--                   | otherwise = '0'
