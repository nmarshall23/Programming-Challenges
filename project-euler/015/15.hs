-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- 
--  =====
--  | | !
--  ----!
--  | | !
--  ----> go over 2 and down 2 [(2,2)]
--
--  ===--
--  | ! |
--  --==!
--  | | !
--  ----> go over 1 and down 1 then over 1 and down 1 [(1,1),(1,1)]
--
-- go over 1 and down 2 then over 1
-- go over 0 and down 1 then over 2 and down 1
-- go over 0 and down 2 then over 2
-- 
-- How many routes are there through a 2020 grid?

--for s = 2 [h,v]
-- [[2,2],[2,2]] ~ [(2,2)]
-- [[1,2],[2,2]] ~ [(1,1),(1,1)] 

nRoutes :: Int -> Int -> [(Int,Int)]
nRoutes _ 0 = []
nRoutes 0 _ = []
nRoutes h v = (h,v) : nRoutes (h - 1) v 
