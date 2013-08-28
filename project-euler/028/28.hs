-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- 1
-- 0
-- 3 5 7 9
-- +2
-- 13 17 21 25
-- +4
-- 31 37 43 49 
-- +6 
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

mkSpiral n = ((n*4) + 4)

spiralWheel [] _ = []
spiralWheel (x:xs) lastNum = turn ++ (spiralWheel xs $ last turn)
	where turn = spiralNums lastNum x
               
spiralNums x n = take 4 [x+(n*2),x+(n*4) ..  ]



diagonalsSum d = (sum $ spiralWheel [1 .. d] 1) + 1
-- 101 ~ diagonalsSum 2

ep28 = diagonalsSum 500
