-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.List

factorial :: (Num n ) => n -> n
factorial 1 = 1
factorial 0 = 1
factorial n = n * factorial (n - 1)

--factList :: [Int]
--factList = map factorial [1 .. 9]
--isSumFactorial :: (Num n) => n -> Bool
--isSumFactorial n = 

------------------------
-- Sulution 
--   take each of fl
--   add them to fl

p34 = filter (\ a -> p34Helper a ) $ filter (\ a -> (length a) > 2) $ subsequences [1 .. 9] --factList
	 
p34Helper a
 | (sumof == digs ) = True
 | otherwise = False
	where sumof = sort $ makeList $ sum (map factorial a)
	      digs  = sort a

------------------------
-- make a list of numbers
-- stolen from http://projecteuler.net/index.php?section=forum&id=4 elt
--
makeList :: (Integral a) => a -> [a]
makeList 0 = [] 
makeList a = (makeList (div a 10)) ++ [(mod a 10)]




