-- Problem 61
-- 
-- Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:
-- 
-- Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
-- Square	 	P4,n=n2	 		1, 4, 9, 16, 25, ...
-- Pentagonal	 	P5,n=n(3n-1)/2	 	1, 5, 12, 22, 35, ...
-- Hexagonal	 	P6,n=n(2n-1)	 	1, 6, 15, 28, 45, ...
-- Heptagonal	 	P7,n=n(5n-3)/2	 	1, 7, 18, 34, 55, ...
-- Octagonal	 	P8,n=n(3n-2)	 	1, 8, 21, 40, 65, ...
--
-- The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
-- 
-- The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).
-- Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), is represented by a different number in the set.
-- This is the only set of 4-digit numbers with this property.
-- Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: 
-- 	triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.

import Control.Monad
import Data.List
import Char
import Data.Ord
import Control.Monad.State


--
--
--
-- PolyNum decrations
--

polygonalFuncs2Types :: [(PolygonalType,(Int->Int))]
polygonalFuncs2Types = [(Triangle,triangleNum),(Square,squareNum),(Pentagonal,pentagonalNum),(Hexagonal,hexagonalNum),(Heptagonal,heptagonalNum),(Octagonal,octagonalNum)]

data PolygonalType = Triangle| Square| Pentagonal| Hexagonal| Heptagonal| Octagonal
	deriving (Eq, Show)


--
-- number spliting and comparing 
--

fst2 = read . (take 2) . show		
lst2 = read . (drop 2) . show

-- is last two numbers of A == to first two numbers of B?
(##=##) :: Int -> Int -> Bool
(##=##) a b = (lst2 a) == (fst2 b)



--
-- number Functions and helpers
--

numsOfType :: PolygonalType -> (Int, Int) -> [Int]
numsOfType pt (upperB, lowerB) = ptFunc
	where ptFunc = case (lookup pt polygonalFuncs2Types) of
			Nothing -> []
			Just f  -> bounds $ map f [1 ..]
	      bounds = (filter (\ a -> (lst2 a) > 9) . takeWhile(< upperB ) . dropWhile (< lowerB ))
	

--
--
--

--main = do putStrLn $ show $ p61Ex1

--
-- p61Ex1
--

type PolyNum = (PolygonalType, Int)

data NumSet = NumSet { num :: Int 
                     , pt  :: PolygonalType
                     , pn  :: Int
                     } deriving (Show)
                       

data NumsMatched = NumsMatched  { nums :: [PolyNum]
				, types :: [PolygonalType]
				} 
				deriving (Show)

--data TypesState = State TypesLeft

p61Ex1 types = do let t = head types
		  ns <- numsOfType t (10000,1000)
		  t0 <- (delete t types)
		  m1 <- matcher ns t0
		  t1 <- (types \\ [t0,t] )
		  m2 <- matcher m1 t1
		  t2 <- (types \\ [t1,t0,t] )
		  m3 <- matcher m2 t2
		  t3 <- (types \\ [t2,t1,t0,t] )
		  m4 <- matcher m3 t3
		  t4 <- (types \\ [t3,t2,t1,t0,t] )
		  m5 <- matcher m4 t4
	          guard ( m5 ##=## ns)
		  return [(t,ns),(t0,m1),(t1,m2),(t2,m3),(t3,m4),(t4,m5)]
		  
p61Ex2 types = do let t  = head types
		  ns <- numsOfType t (10000,1000)
		  m1 <- matcher' [(t,ns)] types
		  m2 <- matcher' m1 types
		  m3 <- matcher' m2 types
		  m4 <- matcher' m3 types
		  m5 <- matcher' m4 types
		  let e = snd $ head m5
	          guard ( e ##=## ns )
		  return $ reverse m5

p61Ex2' (t:ts) = res
	      where res = map (\ n -> p61Ex2'helper ts (t,n) ) (numsOfType t (10000,1000))


p61Ex2'helper :: [PolygonalType] -> PolyNum -> [[PolyNum]]
p61Ex2'helper types ns = filter check $ foldM matcher' [ns] (replicate (length types) types)
		where check rs = ( (snd $ head rs) ##=## (snd $ last rs))
	

--mtTypes :: Int -> [PolygonalType]
--mtTypes n ps = do p <- ps
 --                 m <- matcher n p
                  

{--

mtrWST :: Int -> [PolygonalType] -> StateT [PolygonalType] Maybe [Int]
mtrWST _ [] = do st <- get
		 return st
mtrWST n ts = do st <- get
		 let ts' = ts \\ st
		 let ms = map (\ a -> (a,matcher m a)) ts'
		 
p61wST = p61Helper types
	where types = [Triangle,Square,Pentagonal]

p61Helper types = 
	where triNums = numsOfType (head types) (10000,1000)
		     
 
foldmatches :: [PolygonalType] -> PolyNum -> [[PolyNum]]
foldmatches types sv = foldM matcher' [sv] tts
	where tts = replicate (length types) types

p61Ex1 [PolygonalType] -> Maybe [Int]
p61Ex1 types  = case (runStateT (foldMatch types) startST ) of
	Nothing -> Nothing
	Just (rst,_) -> Just rst
	where types    = [Triangle,Square,Pentagonal]
--}    
      

{--
mtrWST :: Int -> StateT NumsMatched Maybe [Int]
mtrWST n = do st <- get
	      let (ns,pts) = mtr n (types st)
	      put (NumsMatched { types = pts, nums=[]})
	      return ns

mtr :: Int ->  [PolygonalType] -> Maybe ([Int],[PolygonalType])
mtr n types = 
	 case ms of
		[] -> Nothing 
		otherwise -> (ms,ts)
	where ms = matcher n t

--}      
--mrst :: Int -> [PolygonalType] -> Maybe ([Int],[PolygonalType])
mrst :: Int -> [PolygonalType] -> [Maybe ([Int],[PolygonalType])]
mrst m ts = map mms ts
	where mms t = do ms <- maybeMatcher m t
			 Just (ms,(delete t ts))

	    
mstwr m = do st <- get
	     let rm a = map (put) (mrst m a)
	     let f = map rm st
	     return f
	     


--mapM (mrst m )) (replicate (length st) st)
	     

{--
mstwr :: Int -> StateT [PolygonalType] Maybe [Int]
mstwr m = StateT (\st -> mrst m st)

runMatches :: [Int] -> [PolygonalType] -> Maybe [[Int]]
runMatches inums types =  case (runStateT (mapM mstwr inums) types) of
                               Nothing -> Nothing
                               Just (rstr,_) -> Just rstr    
    
--}

matcher' :: [PolyNum] -> [PolygonalType] -> [[PolyNum]]
matcher' pns [] = [pns]
matcher' pns ts = map (\ a -> a:pns)  $ concatMap (numsOfType' b) fts
	where b   = (lb + 100, lb)
	      lb  = (lst2 n) * 100
	      n   = (snd . head) pns
	      fts = ts \\ (map fst pns)  

numsOfType' :: (Int,Int) -> PolygonalType -> [PolyNum]
numsOfType' b t = map (\ a -> (t,a)) (numsOfType t b)

maybeMatcher :: Int -> PolygonalType -> Maybe [Int]
maybeMatcher m t = if null nts
		then Nothing
		else Just nts
	where ub  = lb + 100
	      lb  = (lst2 m) * 100
	      nts = numsOfType t (ub,lb)

--matcher :: Int -> State TypesLeft [Int]
matcher :: Int -> PolygonalType -> [Int]
matcher m t = numsOfType t (ub,lb)
	where 
		ub = lb + 100
		lb = (lst2 m) * 100


----------

--findMatches :: [PolygonalType] -> Int -> StateT Maybe [PolygonalType] [Int]
--findMatches []    = do st <- get
--		       return st
--findMatches types n = do 
--			 findMatches (delete t types) m 

	--where firstTypeSet = numsOfType (head types) (10000,1000)



	 --let pts   = [Triangle,Hexagonal,Heptagonal,Octagonal]
	 --let pts   = [Square,Pentagonal]
	 --let pts   = [Triangle]
	 --let pts   = [Square, Pentagonal, Hexagonal, Heptagonal, Octagonal]


type PolyState = (PolygonalType,Int) 
type PolyValue = Int

startState = (Triangle,8128)


--
-- Polynumber genrating functions
--

triangleNum :: Int -> Int
triangleNum n = ((n * (n +1)) `div` 2)

squareNum :: Int -> Int
squareNum n = n ^ 2

pentagonalNum :: Int -> Int
pentagonalNum n = ((n * (( n * 3) - 1)) `div` 2)

hexagonalNum :: Int -> Int
hexagonalNum n = n * (( n * 2 ) - 1)

heptagonalNum :: Int -> Int
heptagonalNum n = (n * (( 5 * n) - 3)) `div` 2

octagonalNum :: Int -> Int
octagonalNum n = n * ((3 * n) - n )





