{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import Text.Printf
import Data.Vector.Generic ((!))
import qualified Data.Vector.Unboxed as U
import Data.List

readInts :: IO [Int]
readInts = map fst . mapMaybe C.readInt . C.words <$> C.getLine

readIntegers :: IO [Integer]
readIntegers = map fst . mapMaybe C.readInteger . C.words <$> C.getLine

main :: IO ()
main = do
  cases <- readLn
  forM_ [1..cases::Int] $ \cn -> do
    n <- readLn :: IO Int
    as <- readInts

    let calc [] = 0
        calc xs =
          let mi = minimum xs
              (miix:_) = elemIndices mi xs
          in min miix (length xs - 1 - miix) + calc (filter (/= mi) xs)

    printf "Case #%d: %d\n" cn $ calc as

{-
(995,
[927,403,364,267,125,259,277,387,596,995]
)
[25,24,23,22,21,20,20,20,20,20]
Case #18: 20


[927,995]
4+4+3+3+3+2+2+1+1
-}