-- Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
--
-- 37 36 35 34 33 32 31
-- 38 17 16 15 14 13 30
-- 39 18  5  4  3 12 29
-- 40 19  6  1  2 11 28
-- 41 20  7  8  9 10 27
-- 42 21 22 23 24 25 26
-- 43 44 45 46 47 48 49
--
-- It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.
--
--If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

import Data.Ratio

spiralWheel [] _ = []
spiralWheel (x:xs) lastNum = turn ++ (spiralWheel xs $ last turn)
	where turn = spiralNums lastNum x
              spiralNums x n = take 4 [x+(n*2),x+(n*4) ..  ]

primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes'        = p : filter isPrime candidates
    isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
    divides n p    = n `mod` p == 0

isPrime::Integer->Bool
isPrime n =
        let
        sqrt' = truncate $ sqrt (fromIntegral n)
        test [] = True
        test (x:xs) = if mod n x == 0
                      then False
                      else test xs
        in
        test [2..sqrt']


--ratioDiaPrime :: Integer -> 
ratioDiaPrime l = round ((numOfP / numOfS ) * 100 )
	where spiral = take l $ spiralWheel [1 .. ] 1
	      numOfP = fromIntegral $ length $ filter isPrime $ spiral
	      numOfS = fromIntegral $ length $ 1:spiral



