-- http://projecteuler.net/index.php?section=problems&id=29

import Data.List

maxInt = 10


intBcomb m b = map (^b) [2..m]
intAcomb m = concatMap (intBcomb m) [2 ..m]

cleanComb m = length $ nub $ intAcomb m


