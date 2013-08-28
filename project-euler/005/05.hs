-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

divisible :: (Integral a) => a -> [a] -> [Bool]
divisible _ [] = []
divisible c (a:as) = 
	if (c `mod` a) == 0
		then True:(divisible c (as))
		else False:[]

isDivisible :: (Integral a) => a -> Bool
isDivisible a = all (==True) $ divisible a [20,19 .. 1]

isDivisible' :: (Integral a) => [a] -> a -> Bool
isDivisible' tns a = all (==True) $ divisible a tns

smallestDivider :: Integer
smallestDivider = minimum $ filter (isDivisible) [1 .. 10000]


smallestD _ [] = []
smallestD y (x:xs)
	| isDivisible' y x = x:smallestD y xs
	| otherwise = smallestD y xs

-- when I did a scan on 1..10 I saw 5040 which is 2 * 2520
-- Figured could find a number that was a factor then work my way down
upperBounds y = minimum $ filter (isDivisible' y ) $ scanl1 (*) y

findLower y x
	| x == d = x:[]
	| isDivisible' y x = x:findLower y d
	| otherwise = []
	where d = minimum $ filter ( isDivisible' y ) $ map (x `div` ) y


-- this found it.
by20 = findLower y $ upperBounds y
	where y = [20,19.. 1]



