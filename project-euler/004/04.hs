
-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of 
-- two 2-digit numbers is 9009 = 91  99.

-- Find the largest palindrome made from the product of 
-- two 3-digit numbers.

-- started with alg from wikipedia but that was a wash


-- stoled ideas from sumDigits from P16
numToList :: (Integral a) => a -> [a]
numToList 0 = [] 
numToList a = (a `mod` 10):numToList (a `div` 10) 

evenPal :: (Eq a) => [a] -> Bool
evenPal (ns)
	| length ns == 1 = True
	| (length ns == 2) && (headCheck) = True
	| headCheck = evenPal $ init $ tail ns
	| otherwise = False
	where headCheck = head ns == last ns

isPalindrome :: Integer -> Bool
isPalindrome = evenPal . numToList 

nums :: [Integer]
nums = [1 .. 9]



mkrNchains :: [[Integer]]
mkrNchains = map p nums
	where p x = map (* x) nums

chainTester :: [Integer] -> [Integer]
chainTester xs = filter isPalindrome xs

chainBraker :: [[Integer]] -> [Integer]
chainBraker []   = []
chainBraker (x:xs) =  chainTester x ++ chainBraker xs

--stole from http://www.haskell.org/haskellwiki/Introduction#Quicksort_in_Haskell
qsort []     = []
qsort (x:xs) = [x] ++ qsort (filter (> x) xs)

largestPal = last $ qsort $chainBraker $ mkrNchains


------------------------------
-- from the Form P04
-- http://projecteuler.net/index.php?section=forum&id=4

palindrome = maximum (filter (isPalindrome1) [a*b | a <- [100..999], b <- [a..999]]) 
	where 
	isPalindrome1 a = a == merge (makeList a) 

merge [] = 0 
merge (x:xs) = x + merge (map (\x -> x*10) xs) 

makeList 0 = [] 
makeList a = (makeList (div a 10)) ++ [(mod a 10)]
