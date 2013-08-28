
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- stole from  http://www.shlomifish.org/lecture/Perl/Haskell/slides/infinite_lists/primes1.html
primes = sieve [2..] where
    sieve (x:xs) = x:(sieve [a | a <- xs, a `mod` x /= 0 ])

isLPF a p 
	| mod a p == 0 = True
	| otherwise = False

--works but takes a while..
largestPrimeFactor f = filter (isLPF f) $ takeWhile (< (f `div` 71) ) $ primes 

largestPrimeFactor' f = pff f $ primes

pff 			      :: (Integral a) => a -> [a] -> [a]
pff _ [] = []
pff f (x:xs) 
	| filterTest && (isLPF f x)  =  x:pff f xs
	| filterTest          = pff f xs
	| otherwise           = []
	where filterTest = (x < ( f `div` x ))
