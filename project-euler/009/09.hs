-- A Pythagorean triplet is a set of three natural numbers:
--  a <  b < c, for which a^2 + b^2 = c^2

-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc.

isPythagorean (a,b,c)
	| ((a < b) && ( b < c) && ((a^2) + (b^2) == (c^2)) ) = True
	| otherwise = False

isP1k (a,b,c) 
	| a + b + c == 1000 = True
	| otherwise = False

tripleProduct (a,b,c) = a + b + c



makePythagores a b c = filter (isPythagorean) [ (n*a,n*b,n*c) | n <- [1..100] ]

euclidAg m n = (m^2 - n^2, 2 * m * n, m^2 + n^2)

--makeByEuclid = 

walkNumber mkr =(tri,lastNum,pro)
	where tri = mkr $ lastNum
	      pro = tripleProduct $ tri 
	      lastNum = last $  takeWhile (lteq1k mkr ) [1 .. ]
	


lteq1k mkr c
	| isPythagorean tri && tripleProduct tri <= 1000 = True
	| otherwise = False
	where tri = mkr c


makeTriple k = (2*k, k^2 -1 , k^2 +1 )

makeTripleOdd  j k = (j * (2*k +1), 2*j*k*(k+1), j*(2*k*(k+1)+1))
makeTripleEven j k = ( fromEnum $ (j/2) * 2* k,
		       fromEnum $ (j/2)*(k^2 -1),
		       fromEnum $ (j/2)*(k^2 +1))


makeTripleZ z = factor2Triple z $ factorPList f $ factors f
	where f = (z^2) `div` 2

factorPList _ [] = []
factorPList b (a:as) = (a,b `div` a):factorPList b as

factor2Triple _ [] = []
factor2Triple z (a:as) =  (x + z, y + z, z + x + y) : factor2Triple z as
	where x = fst a
	      y = snd a

-- stolen from http://www.haskell.org/haskellwiki/99_questions/31_to_41
factors a = 1: filter (isFactor a) [2..a-1]
isFactor a b = rem a b == 0


-- found with filter isP1k $ concatMap (makeTripleZ ) [2,4..200]
--(200,375,425)
