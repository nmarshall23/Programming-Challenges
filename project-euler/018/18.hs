
import List
import Data.Char


data TriangleRow = TriangleRow {
			row :: [Integer]

			       }deriving (Eq, Show)

mydata = [[3],[7,4]]
datalist = [ [3],[7,4],[2,4,6],[8,5,9,3] ]
triList = map (\ xs -> TriangleRow{row= xs} ) datalist;;


addXnAB x (a:b:xs) = x+a: x+b:[]

--foldListl1                  :: (a -> a -> a) -> [[a]] -> [a]
foldListl1 f (x:xs) = lgo x xs
        where
          lgo a []       = a
          lgo a (b:bs) = lgo (f (head a) (head b) ) bs


walkByHeads c a b =  TriangleRow{row = (row a) ++ myRow c:[] }
	where myRow n = row b !! n

walk2Ints = concatMap  (t) triList
	where t a = mapTris $ row a

mapTris [] = []
mapTris (a:as) = a : mapTris as


---- new effort 2009/21/11

walkT (t:[]) = []
walkT (t:ts) =  wh : walkT ts
	where f = head $ t
	      wh = walkHelper t (head ts) 
	    
-- need to feed it back into it's self

walkHelper [] _   = []
walkHelper _  []  = []
walkHelper (a:as) b
	| length b >= 2 = ( s0, [a, val0] ):( s1 , [a, val1]):wkH 
	| otherwise = []
	where wkH = walkHelper as ( tail b ) 
	      s0 = a + val0  
	      s1 = a + val1 
	      val0 = b !! 0
	      val1 = b !! 1

-- taken from 
--- http://projecteuler.net/index.php?section=forum&id=11&page=2
takeBy n = filter ((n==) . length) . map (take n) . tails
