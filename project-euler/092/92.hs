-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
-- For example,
-- 44  32  13  10  1  1
-- 85  89  145  42  20  4  16  37  58  89
--
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
--
-- How many starting numbers below ten million ( 10,000,000 ) will arrive at 89?

p92 = length $ filter(== 89) $ map numChain [1 .. 9999999]

numChain 89 = 89
numChain 1 = 1
numChain n = numChain $ sum $ map (\ x -> x ^ 2) $ makeList n


makeList 0 = []
makeList a = (makeList (div a 10)) ++ [(mod a 10)]

