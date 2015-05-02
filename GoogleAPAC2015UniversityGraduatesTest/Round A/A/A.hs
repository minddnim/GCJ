import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Bits
import Data.Text as T (pack, unpack, justifyRight)
import Data.Maybe
import Data.Set (fromList, toList)

opSegInf :: [Integer]
opSegInf = cycle [123,127,112,95,91,51,121,109,48,126]

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    (n:inputs) <- words <$> getLine
    let ans = slv (read n :: Int, inputs)
    printf "Case #%d: %s\n" (i :: Int) ans

getOpStr :: Integer -> String
getOpStr num = ((T.unpack . (justifyRight 7 '0') . T.pack) (b2s num))

slvOpStr :: [Integer] -> Integer -> Maybe (Int, Int) -> Maybe (Maybe String)
slvOpStr _ _ Nothing = Nothing
slvOpStr inVals validSeg (Just (start, opNum)) | isOutputOK == 0 = Just (Just (getOpStr opSeg))
                                      | otherwise = Just(Nothing)
  where brokenSegList = zipWith (xor) inVals (drop start opSegInf)
        brokenSeg = foldl (.|.) 0 brokenSegList
        invalidSeg = complement validSeg
        opSeg = (opSegInf !! opNum) .&. (complement brokenSeg)
        isOutputOK = opSeg .&. invalidSeg

slv :: (Int, [String]) -> String
slv (n, inputs) | (and . (map isJust)) opStrs == False = "ERROR!" -- ある一つのパターンについて不確定であったら
                | (length . toList . fromList) opStrs == 1 = (fromJust . head) opStrs -- 解が一つに定まり、さらに出力が確定していたら
                | otherwise = "ERROR!" -- 解が複数あったらエラー
  where inVals = map s2b inputs
        validSeg = foldl (.|.) 0 inVals
        opValidSegInf = map (validSeg .&. ) opSegInf
        opStrs = catMaybes $ map (slvOpStr inVals validSeg) (ansStr n 10 inVals opValidSegInf)

ansStr :: Int -> Int -> [Integer] -> [Integer] -> [Maybe (Int, Int)] -- Maybe (startId, outputId)
ansStr num cnt ips ops = ansStr' num cnt ips ops []
  where ansStr' n 0 _ _ ret = ret
        ansStr' n c ips ops ret | isMatchPtn ips ops == True = ansStr' n (c-1) ips (tail ops) $ (Just (10-c, (10-c)+n `mod` 10)):ret
                                | otherwise = ansStr' n (c-1) ips (tail ops) ret
        isMatchPtn ips ops = and $ zipWith (==) ips ops

s2b :: String -> Integer
s2b str = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) str

b2s ::Integer -> String
b2s v = b2s' v ""
  where b2s' 0 ret = ret
        b2s' val ret = b2s' (val `div` 2) (modChar val:ret)
        modChar n | n `mod` 2 == 1 = '1'
                  | otherwise = '0'
