import Control.Monad
import Text.Printf
import Data.List
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    n <- getLine
    str <- replicateM (read n :: Int) getLine
    printf "Case #%d: %s\n" (i :: Int) (slv str)
 
slv :: [String] -> String
slv strs | (not . checkCorrect) strs = "Fegla Won"
         | otherwise = (show . calcDist) strs

checkCorrect :: [String] -> Bool
checkCorrect strs = checkLength && checkSameKind
  where checkLength = sameElem $ map length groupHead
        checkSameKind = all sameElem groupHead
        groupHead = map (map head) $ transpose $ map group strs

sameElem :: Eq a => [a] -> Bool
sameElem [] = True
sameElem xs = not (any (/= head xs) xs)

calcDist :: [String] -> Int
calcDist strs = sum $ map minDist charNumList
  where charNumList = transpose $ map (map length . group) strs

minDist :: [Int] -> Int
minDist ls = minimum [sum $ map (\x -> abs(x-d)) ls | d <-[minimum ls..maximum ls]]
