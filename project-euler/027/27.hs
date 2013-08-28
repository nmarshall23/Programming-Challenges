-- Problem 27

{--

Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n²  79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

n² + an + b, where |a|  1000 and |b|  1000

	where |n| is the modulus/absolute value of n
	e.g. |11| = 11 and |4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

--}

import Data.List
import Data.Function
import Data.Numbers.Primes
import Control.Applicative


p27 = maxNumPrimes $ primeCount <$> range <*> range 

maxNumPrimes a = maximumBy (compare `on` fst) a

-- trying to spend up the count..

--isP :: (Int,(Integer,[Bool])) -> Bool
pgt0 (c,(a,b))
 | (c == 0) = False
 | otherwise = True

sndF f x = f $ snd x

isEven (a,b)
 | (mod a 2 == 0) && (mod b 2 == 0) = True
 | otherwise = False

--primeF a b = filter (39 < ) $ 
--	where count = length $ helper (quadratic a b) 0

foo = length $ filter (\ x -> 40 < (fst x) ) $ primeCount <$> range <*> range

range  =  [1 .. 999] ++ [-1,-2 .. -999]
--range  = [-1000,-999..0] ++ [1 .. 1000]
range' = [-10,-9 .. 0] ++ [1 .. 100]

--

--primeCount :: Integer -> Integer -> (Int, Integer)
primeCount a b = (count,(a,b))
	where count = length $ helper (quadratic a b) 0
	      coeff = a * b

quadratic :: (Num a) => a -> a -> a -> a
quadratic a b n = (n^2) + (a * n) + b

helper :: (Num a) => (a -> Integer) -> a -> [Integer]
helper f n
 | (isPrime n') = n' : helper f (n+1)
 | otherwise    = []
 where n' = f n


counter = do
	a <- range
	b <- range
	let r = f a b
