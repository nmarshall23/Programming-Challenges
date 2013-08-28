-- Consider the problem of building a wall out of 2x1 and 3x1 bricks (horizontalvertical dimensions) such that,
-- for extra strength, the gaps between horizontally-adjacent bricks never line up in consecutive layers,
-- i.e. never form a "running crack".
--
-- For example, the following 9x3 wall is not acceptable due to the running crack shown in red:
--  3,2,2,2 = 9
--  2,2,3,2 = 9
--  3,3,3   = 9
-- There are eight ways of forming a crack-free 9x3 wall, written W(9,3) = 8.

-- Calculate W(32,10).


-- 3,3,2,2 = 9
-- 3,2,2,3 = 9
-- 2,3,3,2 = 9

-- 3,3,3   = 9
-- 2,2,2,3 = 9
-- 3,2,3,2 = 9

-- 2,2,2,2,2
-- 3,3,3


--type Brick = 2x1 | 3x1

--data Brick = HSize2 = 3 | HSize3 = 2

wall1 = [[3,3,3],[2,2,3,2],[2,2,2,3]]

type Wall = [[Int]]

-- check each layer compare with above layer. If bricks match false, if 
--stableWall :: Wall -> Bool
--stableWall w = 


--mkWall :: Int -> Int -> [Wall]
--mkWall h v = 
			

--brickLayer :: [[Int]] -> [[Int]]
--bricklayer (l:ls) t


max2bricks :: Int -> Maybe [Int]
max2bricks h = maxbrickHelper 3 2 h

max3bricks :: Int -> Maybe [Int]
max3bricks h = maxbrickHelper 2 3 h

maxbrickHelper b1 b2 h
	| mod h  b2 == 0 = Just $  take (divby h)  $ repeat b2
	| mod hm b2 == 0 = Just $ (take (divby hm) $ repeat b2) ++ [b1]
	| otherwise = Nothing
	where divby a = div a b2
	      hm = h - b1

min2bricks _ _ 0 = []
min2bricks b1 b2 h 
	| mod h b1 == 0 =  repeated : min2bricks b1 b2 (h - b2)
	| mod h b1 > 0 && divable h          = b2  : min2bricks b1 b2 (h - b2)
	| h - b2 < 0    = []
	| otherwise     = min2bricks b1 b2 (h - b2) 
	where divable a = div a b1 > 1
	      repeated = (take (div h b1) $ repeat b1 )

