import Control.Monad
import Text.Printf
import Control.Applicative
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [str, n] <- words <$> getLine
    printf "Case #%d: %d\n" (i :: Int) $ slv str (read n)

slv :: String -> Integer -> Integer
slv str n = sumPtn str n 1 ln 0 0
  where ln = (fromIntegral . length) str

sumPtn :: String -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
sumPtn [] _ _ _ _ ret = ret
sumPtn (s:ss) n idx l cnt ret | l < n = ret
                              | isConsonant s && cnt + 1 >= n = sumPtn ss n n (l - rmStrLen) (cnt + 1) (ptn + ret)
                              | isConsonant s = sumPtn ss n (idx + 1) l (cnt + 1) ret
                              | otherwise = sumPtn ss n (idx + 1) l 0 ret
  where rmStrLen = idx - n + 1
        ptn = rmStrLen * (l - (idx - 1))

isConsonant :: Char -> Bool
isConsonant c = c `notElem` "aiueo"