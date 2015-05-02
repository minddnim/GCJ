import Control.Monad
import Text.Printf
import Control.Applicative
import Data.List

type Quaternion = (Integer, Integer, Integer, Integer)

i :: Quaternion
i = (0, 1, 0, 0)

j :: Quaternion
j = (0, 0, 1, 0)

k :: Quaternion
k = (0, 0, 0, 1)

multi :: Quaternion -> Quaternion -> Quaternion
multi (a1, a2, a3, a4) (b1, b2, b3, b4) = (c1, c2, c3, c4)
  where c1 = a1*b1 - a2*b2 - a3*b3 - a4*b4
        c2 = a1*b2 + a2*b1 + a3*b4 - a4*b3
        c3 = a1*b3 + a3*b1 + a4*b2 - a2*b4
        c4 = a1*b4 + a4*b1 + a2*b3 - a3*b2

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [l, x] <- map read . words <$> getLine
    q <- getLine
    let ans = slv (l, x) q
    printf "Case #%d: %s\n" (i :: Int) ans

slv :: (Integer, Integer) -> String -> String
slv (l, n) q | l * n <= 2 || length (group q) < 2 = "NO"
             | answer qPowerVal = ansStr n (take (10000 * 4 * 2) q) -- 入力文字10000文字の4セットの倍見れば大丈夫
             | otherwise = "NO"
  where qPowerVal = power qVal n
        qVal = slvQVal q

ansStr :: Integer -> String -> String
ansStr n q | checkI && checkJ && checkK = "YES"
           | otherwise = "NO"
  where (iq, checkI, it) = dropIJK 'i' ' ' q (n-1, q)
        (ijq, checkJ, ijt) = dropIJK 'j' ' ' iq it
        (ijkq, checkK, ijkt) = dropIJK 'k' ' ' ijq ijt

dropIJK :: Char -> Char -> String -> (Integer, String) -> (String, Bool, (Integer, String))
dropIJK q now [] t@(0, tmp) = ([], q == now, t)
dropIJK q now [] t@(n, tmp) | q == now = ([], True, t)
                            | otherwise = dropIJK q now tmp (n-1, tmp)
dropIJK q now str@(x:xs) t | q == now = (str, True, t)
                           | otherwise = dropIJK q (convMap (now, x)) xs t
  where convMap ('i', 'j') = 'k'
        convMap ('i', 'k') = 'j'
        convMap ('j', 'i') = 'k'
        convMap ('j', 'k') = 'i'
        convMap ('k', 'i') = 'j'
        convMap ('k', 'j') = 'i'
        convMap (_, c) = c

slvQVal :: String -> Quaternion
slvQVal qStr = slvQVal' qStr (1, 0, 0, 0)
  where slvQVal' [] ret = ret
        slvQVal' (q:qs) ret = slvQVal' qs (multi ret (str2Q q))

str2Q :: Char -> Quaternion
str2Q 'i' = i
str2Q 'j' = j
str2Q 'k' = k
str2Q _ = error "no match char"

answer :: Quaternion -> Bool
answer (-1, 0, 0, 0) = True
answer _ = False

power :: Quaternion -> Integer -> Quaternion
power qu n = foldr multi (1, 0, 0, 0) ([(1, 0, 0, 0)] ++ replicate (fromIntegral m) qu)
  where m = n `mod` 4
