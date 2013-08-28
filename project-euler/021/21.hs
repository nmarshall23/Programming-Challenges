
import Control.Monad

pds :: Integer -> [Integer]
pds n = helper 2
	where lim = round $ sqrt $ fromInteger n
	      helper d 
		| d >= lim   = [1]
		| rem == 0  = e ++ (helper (d+1) )
		| otherwise = helper (d+1)
		where rem = n `mod` d
		      d'  = n `div` d
		      e   = if(d == d')
				then [d]
				else [d,d']

--d :: Int -> Int
d = sum . pds

p21 = sum $ do
	a <- [4 .. 10000]
	let b = d a
	guard( a /= b)
	guard( b < 10000)
	guard( a == (d b))
	return a --, b)
