

--main = do putStrLn $ show $ p55

merge [] = 0
merge (x:xs) = x + (merge (map (\ a -> a*10) xs))

makeList 0 = []
makeList a = (makeList (div a 10)) ++ [(mod a 10)]

isPalindrome a = a == merge (makeList a)

reverseAdd a = a + (merge (makeList a))

worker f v tr fr
 |( tr == 50) = True
 |( fr v')    = False
 | otherwise = worker f v' (tr + 1) fr
 where v' = f v


isLychrel a = worker reverseAdd a 0 isPalindrome

--p55 = length $ filter isLychrel [ 100 .. 999]
--p55 = filter isLychrel [ 100 .. 999]


p55 = length $ filter isLychrel [ 5 .. 9999]
