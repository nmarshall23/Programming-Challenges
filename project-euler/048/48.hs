
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series,
--          1^1 + 2^2 + 3^3 + ... + 1000^1000.

import Data.Int	

mkSeries        ::  [Int64] -> [Int64]
mkSeries []     = []
mkSeries (x:xs) = x^x : mkSeries xs

--- Hmm get Int's can't handle numbers this big.
--sumSeries (x:xs) = 

--problem48 s 
