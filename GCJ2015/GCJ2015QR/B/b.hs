{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict          as Map
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
  (d :: Int) <- readLn
  (ps :: [Int]) <- map read . words <$> getLine

  print =<< solve1 ps
  hFlush stdout

solve1 :: [Int] -> IO Int
solve1 ps = do
  let ans = minimum $ map f [1 .. maximum ps]

      f i = i +
            sum
            [ (p + i - 1) `div` i - 1
            | p <- ps
            ]

  return ans

solve2 :: [Int] -> IO Int
solve2 ps = do
  let go :: [Int] -> Int -> Int
      go mm !dep
        -- | trace (prettyShow (mm, dep)) False = undefined
        | null mm = dep
        | dep > 9 = 10
        | otherwise =
          minimum $
          go (filt $ map pred mm) (dep+1) :
          [ go mm' (dep+1)
          | i <- [3..9]
          , i `elem` mm
          , q <- [2..i-2]
          , let mm' = q : (i-q) : (mm \\ [i])
          ]

      filt = filter (> 0)

  return $ go ps 0
