import Control.Monad
import Text.Printf
import Control.Applicative
import qualified Data.Vector as V

main :: IO()
main = do
  testCase <- readLn
  let tbl = createTable 100
  forM_ [1..testCase] $ \i -> do
    [n, k] <- words <$> getLine
    let ans = slv tbl (read n :: Int) (read k :: Integer)
    printf "Case #%d: %s\n" (i :: Int) ans

createTable :: Int -> V.Vector (V.Vector Integer)
createTable n = (V.fromList . (map V.fromList)) tblList
  where tblList = createTableList (n + 1) (concat [[1], replicate n 0])

createTableList :: Int -> [Integer] -> [[Integer]]
createTableList m l = go m l []
  where go 0 _ ret = reverse ret
        go cnt ls ret = go (cnt - 1) (createList (m - (cnt -1)) ls) ((createList (m - (cnt -1)) ls):ret)

createList :: Int -> [Integer] -> [Integer]
createList n l = go n l [0]
  where go _ [] ret = (tail . reverse) ret
        go 0 (x:xs) ret = go 0 xs (x:ret)
        go cnt (x:xs) ret@(y:ys) = go (cnt - 1) xs ((x+y):ret)

slv :: V.Vector (V.Vector Integer) -> Int -> Integer -> String
slv tbl n k | tbl V.! n V.! n < k = "Doesn't Exist!"
            | otherwise = getString tbl n k

getString :: V.Vector (V.Vector Integer) -> Int -> Integer -> String
getString tbl m k = go tbl (m, m - 1) k []
  where go _ (0, _) _ ret = reverse ret
        go tbl (i, -1) idx ret = go tbl (i - 1, -1) idx (')':ret)
        go tbl (i, j) idx ret | tbl V.! i V.! j >= idx = go tbl (i, j - 1) idx ('(':ret)
                              | otherwise = go tbl (i - 1, j) (idx - (tbl V.! i V.! j)) (')':ret)
