import Control.Monad
import Text.Printf
import Control.Applicative
import Data.Map as M
import Data.List as L
 
type Comb = [Integer]
type Combs = [Comb]
type Composite = Integer
type Composites = [Composite]
type Count = Int
 
main :: IO()
main = do
  testCase <- readLn
  printf "Case #1:\n"
  [r,n,m,k] <- Prelude.map read . words <$> getLine
  forM_ [1..testCase] $ \i -> do
    let comb = [[a,b,c,d,e,f,g,h,i,j,k,l]| a<-[2..m],b<-[a..m],c<-[b..m],d<-[c..m],e<-[d..m],f<-[e..m],g<-[f..m],h<-[g..m],i<-[h..m],j<-[i..m],k<-[j..m],l<-[k..m]]
    let memoMap = memo comb
    forM_ [1..r] $ \i -> do
      a <- words <$> getLine
      printf "%s\n" $ intsToStr (slv comb memoMap (strsToInts a))
 
-- Create Memo 
memo :: Combs -> Map Comb (Map Composite Count)
memo cmbs = memo' cmbs M.empty
 
-- Function For Memo
memo' :: Combs -> Map Comb (Map Composite Count) -> Map Comb (Map Composite Count)
memo' [] ret = ret
memo' (x:xs) ret = memo' xs (M.insert x countMap ret)
  where  countMap = cntMap prod
         prod = mkProd x
 
-- Solve Products from Combination
mkProd :: Comb -> Composites 
mkProd x = mkProd' x [1]
  where mkProd' [x] [1] = [1,x]
        mkProd' (x:xs) [1] = mkProd' xs [1,x]
        mkProd' [x] prod = prod ++ (L.map (*x) prod)
        mkProd' (x:xs) prod = (mkProd' xs prod) ++ (mkProd' xs (L.map (*x) prod))
 
-- Composite Count
cntMap :: Composites -> Map Composite Count
cntMap cmp = cntMap' cmp M.empty
  where cntMap' [] ret = ret
        cntMap' (x:xs) ret = cntMap' xs (insertWith' (+) x 1 ret)
 
-- Answer Combination
slv :: Combs -> Map Comb (Map Composite Count) -> Composites -> Comb
slv combs memo a = slv' combs memo a [] 0
 
slv' :: Combs -> Map Comb (Map Composite Count) -> Composites -> Comb -> Count -> Comb
slv' [] memo a ret cnt = ret
slv' (x:xs) memo a ret cnt | cnt < cmbCnt  = slv' xs memo a x cmbCnt
                           | otherwise = slv' xs memo a ret cnt
  where cmbCnt = combCnt a memo x
 
-- Count Combination Meet Conditions 
combCnt :: Composites -> Map Comb (Map Composite Count) -> Comb -> Count
combCnt cmp memo comb = combCnt' cmp (memo!comb) 0
 
combCnt' :: Composites -> Map Composite Count -> Count -> Count
combCnt' [] memo cnt = cnt
combCnt' (x:xs) memo cnt | M.member x memo == True = combCnt' xs memo (cnt + memo!x) 
                         | otherwise = 0 
 
-- Input / Output 
strsToInts :: [String] -> [Integer]
strsToInts [] = []
strsToInts (x:xs) = [(read :: String -> Integer) x] ++ strsToInts xs 
 
intsToStr :: [Integer] -> String
intsToStr xs = concat $ L.map show xs