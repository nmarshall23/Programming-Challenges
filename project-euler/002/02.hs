
-- Bug in my code, Doesn't add the starting two numbers to list.
-- Kind of fixed it with another funtion
fibmkr:: (Num a) => a -> a -> [a]
fibmkr a b = c:(fibmkr b c)
	where c  = a + b

fibonacci :: [Integer]
fibonacci = a:b:(fibmkr a b)
	where a = 1
              b = 2

-- Stolen from Haskell wiki
fibs :: [Integer]
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (1,2)

sumOfEven :: Integer
sumOfEven = sum $ filter even $ takeWhile (< 4000000 ) $ fibs
