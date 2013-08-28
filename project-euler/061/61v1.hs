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


main = do putStrLn $ show $ p61

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

--numList :: Int -> [Int]
numList f = takeWhile(< 10000) $ dropWhile (< 1000 ) $ map f [1 ..]

fst2Num :: Int -> Int
fst2Num = read . (take 2) .  show

snd2Num :: Int -> Int
snd2Num = read . (drop 2) .  show

--p61 :: Int
p61 = sum $ take 6 dowork
	where dowork = do
		  tn <- numList' triangleNum 
		  sn <- numList' squareNum
		  pn <- numList' pentagonalNum
		  hn <- numList' hexagonalNum
		  en <- numList' heptagonalNum
		  on <- numList' octagonalNum
		  r  <- testcycr tn [sn,pn,hn,en,on] testB
		  return r


--isCyclic :: [Int] -> Bool
isCyclic (x:xs)  
 | isnt = False
 | fnlM = False
 | otherwise = True
	where isnt = any (== Nothing) $ flm
              flm  = foldP matchP (x:xs)
	      fnlM = (Nothing == (matchP (maybe 0 id $ last $ flm) [x]))
	

--foldP :: (Num a,  Monad m) => (a -> [a] -> Maybe a) -> [a] -> m [Maybe a]
foldP _ []    = []
foldP _ [x]   = []
foldP f (x:xs)
 | (val == Nothing) = Nothing : []
 | otherwise = val : foldP f (clv:lvf)
	where val = f x xs
              clv = maybe 0 id val
              lvf = filter (/= clv) xs

matchP a bs = msum $ map t bs 
	where t x = if ((snd2Num a) == (fst2Num x))
		    then Just x
		    else Nothing


fp :: (Num a) => (a -> [a] -> [a]) -> [a] -> [a]
fp _ [] = []
fp f (x:xs) = case val of
	[] ->  []
	_  -> val ++ (fp f (val ++ lf) )
	where val = f x xs
	      lf  = filter (/= (head val)) xs


compFS a bs = do
		b <- bs
		guard ((snd2Num a) == (fst2Num b))
		return b

p61e = do
   tn <- numList triangleNum
   sn <- numList squareNum
   pn <- numList pentagonalNum
   let cycSet = [tn,sn,pn]
   guard(isCyclic cycSet )
   return cycSet
  -- let c = [tn,sn,pn]
--   let a = fp compFS c
--   guard ((length a) == 2)
--   let l = compFS (last a) [tn]
--   guard (l == tn)
--   return (tn:a)


p61e' = do
   tn <- numList' triangleNum 
   sn <- numList' squareNum
   pn <- numList' pentagonalNum
   r  <- testcycr tn [sn,pn] testB
   return r

testA a b = (snd2Num a) == (fst2Num b)
testB :: Int -> Int -> Bool
testB a b = a == b

type CycNum = (Int, Int, Int)
type CycSet = [CycNum]

testcycr :: CycNum -> CycSet -> (Int -> Int -> Bool) -> [Int]
testcycr a bs t
 |(hpr == []) = mzero
 |(t a1 b2 ) = (ha:bs')
 |otherwise = mzero 
 where hpr       = helper a bs
       (a1,_,ha) = a
       (_,b2,_)  = toDig $ last $ head hpr
       bs'       = head hpr

helper' :: Int -> [Int] -> [[Int]]
helper' _ [] = return [] 
helper' a bs = do
	b <- bs
	guard ((snd2Num a) == (fst2Num b))
	let p = filter(/= b) bs
	next <- helper' b p
	return (b : next)


helper :: CycNum -> CycSet -> [[Int]]
helper _ [] = return [] 
helper (_,a2,_) bs = do
	b@(b1,_,bn) <- bs
	guard (a2 == b1)
	let p = filter(\ (_,_,x) -> x /= bn) bs
	next <- helper b p
	return (bn : next)


setA :: [Int]
setA = [2346,3293,4623,4632,9346]
setB' :: [Int]
setB' =[8128, 2982, 8281]
setB :: [Int]
setB = [8128, 2882, 8281]


toDig :: Int -> CycNum
toDig a = ((toDigHlr take a), (toDigHlr drop a), a)

toDigHlr :: (Int -> String -> String) -> Int -> Int
toDigHlr f = read . (f 2) .  show



numList' :: (Int -> Int) -> CycSet
numList' numfunc = takeWhile twf $ dropWhile dwf $ map (rv . numfunc) [1 ..]
	where fuc x = read . (x 2) .  show
	      rv a = (fuc take a,fuc drop a,a)
	      twf (_,_,x) = x < 10000
	      dwf (_,_,y) = y < 1000

-- p61 = do
--   tn <- numList triangleNum
--   sn <- numList squareNum
--   pn <- numList pentagonalNum
--   let set1 = matchfs tn [sn,pn] []
--   guard(matchC set1)
--   let set1' = maybe 0 id set1
--   let prd   = leftover set1' [sn,pn]
--   let set2  = matchfs set1' prd []
--   guard(matchC set2)
--   let set2' = maybe 0 id set2
--   let set3 = matchfs set2' (tn:[]) []
--   guard(matchC set3)
--   return [tn,set1',set2']
 

-- p61' = do
--   tn <- numList triangleNum
--   sn <- numList squareNum
--   pn <- numList pentagonalNum
--   p1 <- compareNums tn [sn,pn]
--   let prd   = leftover p1 [sn,pn]
--   p2 <- compareNums p1 prd
--   p3 <- compareNums p2 (tn:[])
--   return [tn,p1,p2]
  
-- compareNums a b = case val of
-- 	Nothing -> mzero
-- 	Just x  -> return x
-- 	where val = matchfs a b []

-- matchC a = case a of
-- 	Nothing -> False
-- 	_	-> True

--leftover :: Integer -> 
-- leftover a bs = filter (/= a) bs

-- matchfs :: Integer -> [Integer] -> [Integer] -> Maybe Integer
-- matchfs _ [] _ = Nothing
-- matchfs a (b:bs) rs
--  |((snd2Num a) == (fst2Num b)) = Just b
--  | otherwise = matchfs a bs (b:rs)



 
 


