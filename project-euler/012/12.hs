-- The sequence of triangle numbers is generated by adding the natural numbers. 
-- So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Let us list the factors of the first seven triangle numbers:
--
--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28
-- We can see that 28 is the first triangle number to have over five divisors.
--
-- What is the value of the first triangle number 
-- to have over five hundred divisors?

import Data.List

triangleNums = scanl1 (+) [1 ..]

--listFactor n = factorHelper n n

--factorHelper f cn
--	| cn == 0 = []
--	| (mod f cn) == 0 = cn : factorHelper f (cn -1 )
--	| otherwise = factorHelper f (cn -1 )


--problem12 a = find (\ b -> a < factorLen b ) $ p12 (a + 1)

--p12 a = takeWhile (factorLen a) triangleNums

p12 d = head $ dropWhile (\a -> d > (factorLen a)  ) $ triangleNums

--p12 500 = 76576500
--my factor code must be slow

factorLen a = genericLength $ factors' a

factors a = 1: filter (isFactor a) [2..a-1]
isFactor a b = rem a b == 0

factors' a = fh a a 1

fh n b c
 | r && (c > d) =  []
 | r && (c == d) = c : []
 | r = c : d : fh n d (c+1)
 | (c > b) = []
 | otherwise = fh n d (c+1)
 where r = rem n c == 0
       d = div n c
