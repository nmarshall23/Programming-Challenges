-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
-- 
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
-- It is possible to make £2 in the following way:
--
-- 1£1 + 150p + 220p + 15p + 12p + 31p
--
-- How many different ways can £2 be made using any number of coins?
--

import Control.Monad


coin = [5,2,1]


coins = [200,100,50,20,10,5,2,1]
total = 200

main = do putStrLn $ show $ chghelper total coins

chghelper _ [] = 0
chghelper tn (c:cs) = mkchg tn (c:cs) + chghelper tn cs

mkchg [] _ = []
mkchg (c:[]) rs
  |(sumCRS == total) = (c:rs)
  |(sumCRS  < total) = mkchg (c:[]) (c:rs)
  |otherwise = []
  where sumCRS = (sum rs) + c
mkchg (c:cs) rs
  |(c == total) = (c:rs):[]
  |(sumCRS == total) = (c:rs)
  |(sumCRS  < total) = (mkchg cs (c:rs)) : ( mkchg (c:cs) (c:rs) )
  |otherwise = []
  where sumCRS = (sum rs) + c

--mkchg _ [] = 0
--mkchg tn (c:cs) 
-- |(chg > 0)  = (mkchg chg cs)+(mkchg chg (c:cs))
-- |(chg == 0) = 1
-- |(chg < 0)  = 0
-- where chg = tn - c

--mkchg' :: a -> [a] -> [a] -> [[a]]
--mkchg' 0 _ rs = rs 
--mkchg' _ [] _ = []
--mkchg' tn (c:cs) rs = (mkchg' chg cs res ):(mkchg' chg (c:cs) res)
-- where chg = tn -c
--       res = (c:rs)

--f tn (c:cs) = h tn (c:cs) ++ f tn cs

--h _ [] = []
--h tn (c:cs)
-- | tn - c == 0 = (c,tn) : []
-- | tn - c >= 0 = (c,tn) : h (tn - c ) ( c:cs)
-- | otherwise = h tn cs

--h' :: (Num a, Ord a) => a -> ( a -> [a] -> [a] ) -> [a] -> [a]
--h' _ _ [] = []
--h' tn f (c:cs)
-- | tn - c == 0 = c : []
-- | tn - c >= 0 = c : ( h' (tn - c ) f ( f c cs ) )
-- | otherwise = h' tn f cs

--h2 _  st [] = st
--h2 0 st _ = st
--h2 tn st (c:cs)
-- | tn - c == 0 = c
-- | tn - c >= 0 = h2 (tn - c ) (c:st) ( c:cs) : h2 (tn - c) (c:st) cs
-- | otherwise = []  -- h2 tn st cs

	       --True <- return(tn - x > 0)
--foo _ [] = return []
--foo tn (c:cs) = 
--	do next <- foo tn cs
--	   let chg = tn - c
--	   case chg of
--	     0 -> return (c : next)
--	     _ -> return (0 : next)
        
--hlpr _ [] = []  
--hlpr tn cs = do c <- cs
--                let chg = tn - c
--                True <- return ( chg > 0)
--		return (c)
	       
--divBy :: Integral a => a -> [a] -> Maybe [a]
--divBy _ [] = return []
--divBy _ (0:_) = fail "division by zero in divBy"
--divBy numerator (denom:xs) =
 --   do next <- divBy numerator xs
  --     return ((numerator `div` denom) : next)

--backtraceEx a b = do x <- a
 --                    y <- b
  --                   guard (x > y)
   --                  return (x,y)

--findChg :: Num a => a -> [a] -> [[a]]
--findChg tn cs = map (hlpr tn) cs

--	case sum(st) == tn of
--	True  = Just st
--	False = Nothing

-- | tn - c >= 0 = c : h (tn - c ) ( c:cs)

-- | tn - c == 0 = []
-- | tn - c >= 0 = c : h (tn - c ) ( c:cs)
-- | otherwise = h tn cs

-- | tn - c == 0 = c : []

--sumCoins :: Int -> [Int] ->  [(Int,[Int])]
--sumCoins l [] = []
--sumCoins l (c:cs)
--	| l - c == 0 = (0,c) : sumCoins l cs  -- (1,[c]):sumCoins l cs
--	| otherwise = ((l -c),c) : sumCoins l cs --sumCoins (l - c) (c:cs) : sumCoins (l - c) cs  : sumCoins l cs
--	| otherwise =  tryNums l (l -c ) cs : sumCoins l cs


--findchangeL2 :: Int
--findchangeL2 = length $ filter (eqL2) $ sumCoins l2 coins
--	where eqL2 a =  (200 == a) 

--f _ _ [] = []
--f _ 0 _  = []
--f s tn (c:cs) = case r of
--		 True  -> (c:s) : f s tn cs
--		 False -> f (c:s) y (c:cs)
--		where r = (y == 0)
--		      y = tn - c


--someFunc :: Int -> [Int] -> [[Int]]
--someFunc _ [] = []
--someFunc tn (c:cs) = helper tn (c:cs) : someFunc tn cs

--helper  :: Int -> [Int] -> [Int]
--helper _ [] = []
--helper 0 _ = []
--helper tn (c:cs) = c : helper r f
--	where r = tn - c
--	      f = (c:cs)


-- someFunc 20 coins20 
-- [[20],[10,10],[5,5,5,5],[2,2,2,2,2,2,2,2,2,2],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

--problem31 = length $ someFun coins


