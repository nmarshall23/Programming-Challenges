-- By starting at the top of the triangle below and moving to adjacent numbers on the row below.
-- The maximum total from top to bottom is 23.
--
--    3
--   7 4
--  2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.

import Data.Tree
import Data.Traversable
import Data.Char

mylist = [ ["3"],["7","4"], ["2","4","6"],["8","5","9","3"]]
mytree = mkTr (head $ head mylist) (tail mylist) 
-- Depth-first tree traversal.
--depthFirst                       :: Tree a -> [a]
--depthFirst (Leaf x)              =  [x]
--depthFirst (Branch x left right) =  depthFirst left ++ [x] ++
 --                                   depthFirst right


-- Define tree
mytree1 = Node{ rootLabel=3,
		subForest=[ Node{ rootLabel=7, subForest=[] }
			  , Node{ rootLabel=4, subForest=[] }
			  ]
              }

mkTr' :: [[a]] -> Tree a
mkTr' (x:[]) =  Node{ rootLabel = head x, subForest=[] }
mkTr' (x:xs) = Node{ rootLabel = head x, subForest=[ mkTr' right, mkTr' left] }
	where children  = head xs
	      right     = [(head children)] : grandkids
	      left      = [(head $ tail children)] : grandkids
	      grandkids = tail xs


--fdch            :: [[a]] -> (a -> [a] -> t) -> [[t]] 
fdch (x:[]) a   = eachX' x [] a : []
fdch (x:y:xs) a = eachX' x y a  : fdch (y:xs) a

eachX'                   :: [a] -> [a] -> (a -> [a] -> t) -> [t]
eachX' [] _ _            = []
eachX' (x:xs) [] r       = r x [] : eachX' xs [] r
eachX' (x:xs) (a:b:as) r = r x [a,b] : eachX' xs (b:as) r

--mkTr :: [a] -> [[t]] -> Tree a
mkTr f []     = Node{ rootLabel =f, subForest = [] }
mkTr f (x:[]) = Node{ rootLabel =f, subForest = (subF x [] ) }
mkTr f (x:xs) = Node{ rootLabel =f, subForest = (subF x xs) }
--(subF x xs )   }
--	where sub (a:b:as) = mkTr a (xs): mkTr b (xs):[]



--subF                   :: [a] -> [a] -> (a -> [a] -> t) -> [t]
--subF :: [a] -> [[a]] -> Forest a
--subF [] (a:as) = mkTr th [] : subF [] as
--	where th = head a
--subF (x:xs) []     = mkTr x [] : subF xs []
--subF (x:xs) (a:[]) = mkTr x [th] : subF xs ch:[]
--	where ch = tail a
--	      th = (head a): (head ch) : []

--subF (x:xs) (a:as) = mkTr x (th:as) : subF xs (ch:as)
--	where ch = tail a
--	      th = (head a): ( head ch) : []

--subF [] _        = []
--subF (x:xs) (a:as) = mkTr x [] : subF xs as

--subF' :: [a] -> [[t]] -> Tree a
subF [] _ = []
subF (x:xs) [] = mkTr x [] : subF xs []
subF (x:xs) ((f:n:as):bs) = mkTr x ([f,n]:bs) : subF xs ((n:as):bs)
--subF (x:xs) ((f:n:as):bs) = ( x , ([f,n]:bs) ) : subF xs ((n:as):bs)


mkDrawin = putStr $drawTree mytree
