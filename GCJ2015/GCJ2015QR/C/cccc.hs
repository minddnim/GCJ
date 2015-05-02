{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Debug.Trace
import           System.IO
import           Text.Printf
import Control.Applicative

main :: IO ()
main = do
  cases <- readLn
  forM_ [1..cases] $ \(caseno :: Int) -> do
    printf "Case #%d: " caseno
    solve

solve :: IO ()
solve = do
  [l, x :: Int] <- map read . words <$> getLine
  s <- getLine

  let v = qpow (foldl' f 1 s) x

      ans = isJust $ do
        s' <- search (-2) 100000 $ take (l * x) $ cycle s
        _ <- search (-3) 100000 s'
        return ()

  putStrLn $ if v == -1 && ans then "YES" else "NO"

search :: Int -> Int -> String -> Maybe String
search 1 _ ss = Just ss
search _ 0 _ = Nothing
search _ _ [] = Nothing
search !acc !rest (s:ss) =
  search (f acc s) (rest - 1) ss

f :: Int -> Char -> Int
f 1 'i' = 2
f 1 'j' = 3
f 1 'k' = 4

f 2 'i' = -1
f 2 'j' = 4
f 2 'k' = -3

f 3 'i' = -4
f 3 'j' = -1
f 3 'k' = 2

f 4 'i' = 3
f 4 'j' = -2
f 4 'k' = -1

f a c = -1 * f (-a) c

qpow a b
  | a == 1 = 1
  | a == -1 = a ^ (b `mod` 2)
  | otherwise =
    [1, a, -1, -a] !! (b `mod` 4)
