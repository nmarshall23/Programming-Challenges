
-- http://projecteuler.net/index.php?section=problems&id=79

type LoginBit = Integer

data LoginData = LoginData {a ::LoginBit, b ::LoginBit, c ::LoginBit }
	deriving (Eq,  Show)

class (Eq a) => OrdData a where
	(<!), (!>) :: a -> a -> Bool
	(<!!), (!!>) :: a -> a -> Bool
	(<!!!), (!!!>) :: a -> a -> Bool
	
instance OrdData LoginData where
	(LoginData a1 b1 c1) <!   (LoginData a2 b2 c2) = (a1 < a2)
	(LoginData a1 b1 c1) <!!  (LoginData a2 b2 c2) = (b1 < b2)
	(LoginData a1 b1 c1) <!!! (LoginData a2 b2 c2) = (c1 < c2)
	(LoginData a1 b1 c1) !>   (LoginData a2 b2 c2) = (a1 > a2)
	(LoginData a1 b1 c1) !!>  (LoginData a2 b2 c2) = (b1 > b2)
	(LoginData a1 b1 c1) !!!> (LoginData a2 b2 c2) = (c1 > c2)




someData = [317,518,127,328]

mkhelper 0 = []
mkhelper x = x `mod` 10 : mkhelper ( x `div` 10 )

mkBits [] = []
mkBits (x:xs) = LoginData{a = bits!!0 ,b = bits!!1 ,c= bits!!2 } : mkBits xs
	where bits = reverse $ mkhelper x

--compareBits a b 
