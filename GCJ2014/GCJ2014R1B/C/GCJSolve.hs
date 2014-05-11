import Control.Monad
import Control.Arrow
import Text.Printf
import Control.Applicative
import qualified Data.Map as M
import Data.List (sort, delete)

type City = Int
type ZipCode = Int
type CodeTbl = M.Map City ZipCode
type AdjList = M.Map ZipCode [ZipCode]

main :: IO()
main = do
  testCase <- readLn
  forM_ [1..testCase] $ \i -> do
    [n, m] <- map read . words <$> getLine
    zipCodes <- map read <$> replicateM n getLine
    edges <- replicateM m $ (\xs -> map read $ words xs :: [Int]) <$> getLine
    let codeTbl = M.fromList $ zip [1..n] zipCodes
        zipCodeEdges = map (map (codeTbl M.!)) edges 
        outboundFlight = map (head &&& tail) zipCodeEdges
        returnFlight = map ((head &&& tail).reverse) zipCodeEdges
        flightAdjList = M.map sort $ M.fromListWith (++) $ outboundFlight ++ returnFlight
    printf "Case #%d: %s\n" (i :: Int) $ slv n codeTbl flightAdjList
 
slv :: Int -> CodeTbl -> AdjList -> String
slv n tbl obLs | M.size tbl == 1 = show $ tbl M.! 1
               | otherwise = concatMap show $ slvZipList n obLs (sort (M.keys obLs)) []

slvZipList :: Int -> AdjList -> [ZipCode] -> [[ZipCode]] -> [ZipCode]
slvZipList _ _ [] ret = minimum ret
slvZipList n obLs (start:starts) ret | length ansZipCode == n = slvZipList n obLs starts (ansZipCode:ret)
                                     | otherwise = slvZipList n obLs starts ret
  where ansZipCode = dfs obLs reLs [start] (obLs M.! start) []
        reLs = M.fromList $ zip (M.keys obLs) (repeat [])

dfs :: AdjList -> AdjList -> [ZipCode] -> [ZipCode] -> [ZipCode] -> [ZipCode]
dfs _ _ [] [] _ = error "Nothing solve."
dfs _ _ [_] [] ans = reverse ans
dfs obLs reLs (_:nps) [] ans = dfs obLs reLs nps (sort (obLs M.! head nps ++ reLs M.! head nps)) ans
dfs obLs reLs nowPts (nextPt:nextPts) ans | nextPt `elem` ans = dfs obLs reLs nowPts nextPts ans
                                          | otherwise = dfs obLs' reLs' (nextPt:nowPts) (sort (obLs M.! nextPt ++ reLs M.! nextPt)) (nextPt:ans)
  where obLs' = M.adjust (delete nextPt) (head nowPts) obLs
        reLs' = M.insertWith (++) nextPt [head nowPts] reLs
