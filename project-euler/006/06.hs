-- The sum of the squares of the first ten natural numbers is,
-- 12 + 22 + ... + 102 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)2 = 552 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- http://projecteuler.net/index.php?section=problems&id=6

sumOfSq :: (Num a, Enum a) => a -> a
sumOfSq x = sum $ map (^2) [1 .. x]

sqOfsum :: (Num a, Enum a) => a -> a
sqOfsum x = ( sum [1 .. x] ^ 2)

difference a = (sqOfsum a) - (sumOfSq a)
