
-- stolen from http://www.haskell.org/haskellwiki/The_Fibonacci_sequence

fib 0 = 0
fib 1 = 1
fib n | even n         = f1 * (f1 + 2 * f2)
      | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
      | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib k
         f2 = fib (k-1)


-- stolen from http://projecteuler.net/index.php?section=forum&id=20
countDigits :: Integer -> Integer 
countDigits 0 = 0 
countDigits n = 1 + countDigits (n `div` 10)

--tec 517-721-2837

-- We are one off because testLT only graps the one just before it
-- finds the number of digits
problem25 n = (1 +) $ length $  takeWhile (testLT  (n - 1))  $ map (fib ) [ 1..]
	where testLT = (\ n x -> ( (countDigits x) <= n ))
