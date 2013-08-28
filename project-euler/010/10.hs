	
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
-- ie 2000000


findFactors :: (Integral a) => a -> [a]
findFactors x = factorHelper x x
 
factorHelper _ 0 = []
factorHelper f n
	| f `mod` n == 0 = n:factorHelper f (n - 1) 
	| otherwise = factorHelper f (n - 1) 

isPrime x
	| head fl == x && last fl == 1 && length fl == 2 = True
	| otherwise = False
	where fl = findFactors x

sumOfPrimes x = sum $ filter (isPrime') [x, x -2 .. 1]

--

isFactor x y
	| x `mod` y == 0 = True
	| otherwise = False

isPrime' x
	| fl > 2 = False
	| otherwise = True
	where fl = length $ limitFilter (isFactor x) 2 [x, x -1 .. 1]

limitFilter _ _ []  = []
limitFilter _ 0 (x:xs) = x :[] 
limitFilter t i (x:xs)
	| (t x) && i > 0 = x: limitFilter t (i - 1) xs 
	| otherwise = limitFilter t i xs
	 

-- stole from http://www.haskell.org/haskellwiki/Prime_numbers#Prime_Wheels

primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes'        = p : filter isPrime candidates
    isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
    divides n p    = n `mod` p == 0

sumOfPrimes' x = sum $ takeWhile (x >) primes
-- x = 2000000, 142913828922


-- from http://projecteuler.net/index.php?section=forum&id=10&page=2
isPrime''::Integer->Bool 
isPrime'' n = 
	let 
	sqrt' = truncate $ sqrt (fromIntegral n) 
	test [] = True 
	test (x:xs) = if mod n x == 0 
		      then False 
		      else test xs 
	in 
	test [2..sqrt'] 

sumOfPrimes'' x = sum . filter isPrime'' $ [2..x] 
