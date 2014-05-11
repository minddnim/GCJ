import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Bits
import Data.List
 
main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [n, l] <- map read . words <$> getLine
    x <- getLine
    y <- getLine
    printf "Case #%d: %s\n" (i :: Int) (slv n l (words x) (words y))

slv :: Int -> Int -> [String] -> [String] -> String
slv _ _ x y = ansCount (map b2i x) (sort (map b2i y))

ansCount :: [Integer] -> [Integer] -> String
ansCount xi yi = flipMinimumNum ansListXi yi []
  where possibleFlipAns = map (xor (head xi)) yi
        ansListXi = createAnsList possibleFlipAns xi []

createAnsList :: [Integer] -> [Integer] -> [([Integer], Integer)] -> [([Integer], Integer)]
createAnsList [] _ ret = ret
createAnsList (f:fs) xi ret = createAnsList fs xi ((sort (map (xor f) xi), f):ret)

flipMinimumNum :: [([Integer], Integer)] -> [Integer] -> [Int] -> String
flipMinimumNum [] _ ret | null ret = "NOT POSSIBLE"
                        | otherwise = (show . minimum) ret
flipMinimumNum (a:as) yi ret | fst a == yi = flipMinimumNum as yi (popCount (snd a):ret)
                             | otherwise = flipMinimumNum as yi ret

b2i :: String -> Integer
b2i xs = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) xs