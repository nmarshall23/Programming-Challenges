
import Data.List
import Control.Monad

--helper :: Int -> [Int] -> [[Int]]
--helper :: (Monad m) => Int -> [Int] -> m [Int]
helper gs m@(x:xs) = do
	a <- [x .. gs]
	return (a:m)

--grouper :: Int -> [[Int]]
grouper gs = do
	a <- [0 .. gs]
	return [a]

gridSize2 = length $ do
	let gs = 2
	a <- [0 .. gs]
	helper gs [a]

gridSize3 = length $ do
	let gs = 3
	a <- [0 .. gs]
	b <- helper gs [a]
	helper gs b

gridSize2' = length $ f 2
	where f size = grouper size >>= helper size 

gridSize3' = length $ f 3
	where f size = [0 .. size] >>= (\ a -> (helper size [a]) ) >>= helper size

--gridAnySize :: (Monad m) => Int -> 
gridAnySize size = length $ grouper size >>= foldr (<=<) return (replicate (size - 1) (helper size))


