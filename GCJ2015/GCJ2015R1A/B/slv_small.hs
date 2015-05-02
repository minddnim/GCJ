import Control.Monad
import Text.Printf
import Control.Applicative
import qualified Data.List as L
import qualified Data.Vector as V

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [b, n] <- words <$> getLine
    m <- map read.words <$> getLine
    let ans = slv (read b :: Int) (read n :: Integer) m
    printf "Case #%d: %d\n" (i :: Int) ans

slv :: Int -> Integer -> [Integer] -> Int
slv b n m | modNth == 0 = getId m
          | otherwise = getStoreId modNth m mListLcm b
  where mListLcm = foldl lcm 1 m
        mSumCnt = L.sum $ L.map (mListLcm `div`) m
        modNth = n `mod` mSumCnt

getId :: [Integer] -> Int
getId (mTime:mTimes) = go 2 mTimes (1, mTime)
  where go _ [] ret = fst ret
        go n (m:ms) (mId, time) | time >= m = go (n+1) ms (n, m)
                                | otherwise = go (n+1) ms (mId, time)

getStoreId :: Integer -> [Integer] -> Integer -> Int -> Int
getStoreId nth m minLcm b = makeList nth (V.fromList m) (V.fromList (L.replicate b minLcm)) 0

makeList :: Integer -> V.Vector Integer -> V.Vector Integer -> Int -> Int
makeList nth bTimeVec vec ret | nth == 0 = ret + 1
                              | otherwise = makeList (nth - 1) bTimeVec modVec maxId
  where maxId = V.maxIndex vec
        maxTime = vec V.! maxId
        modVec = vec V.// [(maxId, maxTime - bTimeVec V.! maxId)]
