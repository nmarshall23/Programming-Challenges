-- n! means n x (n - 1)  ...  3 x 2 x 1
-- Find the sum of the digits in the number 100!
-- http://projecteuler.net/index.php?section=problems&id=20

import Data.Char

bang :: (Num a, Enum a) => a -> a
bang a = foldl1 (*) [a, (a - 1) .. 1]


sumOfbang :: (Num a, Enum a) => a -> Int
sumOfbang a = sum $ map (digitToInt ) 
		$ show $ bang a


-- Checking my work, stolen from P20 form
fac :: Integer -> Integer 
fac 0 = 1 
fac n = n * fac (n-1)
