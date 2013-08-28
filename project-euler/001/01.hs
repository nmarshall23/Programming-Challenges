
isTN x 
  | ((mod x 3) == 0) = True
  | ((mod x 5) == 0) = True
  | otherwise = False


sumFrom :: (Integral a) => a -> a
sumFrom x = sum $ filter isTN [1 .. x]

main = do
  putStrLn ("Sum: " ++ (show (sumFrom 999) ) )
