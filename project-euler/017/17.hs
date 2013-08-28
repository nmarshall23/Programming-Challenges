--If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
--If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


--data NumberWord = Numbers1to19 | NumbersTens | And

type NumberWords = [Numbers]

data Numbers = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen | Sixteen | Seventeen | Eighteen | Nineteen | Twenty | Thirty | Fouty | Fifty | Sixty | Seventy | Eighty | Ninety | Hundred | Thousand | And
	deriving (Eq, Ord, Enum, Read, Show )

numbers1to19 :: [Numbers]
numbers1to19 = [One , Two , Three , Four , Five , Six , Seven , Eight , Nine, Ten , Eleven , Twelve , Thirteen , Fourteen , Fifteen , Sixteen , Seventeen , Eighteen , Nineteen ]

numbersTens :: [Numbers]
numbersTens = [ Ten , Twenty , Thirty , Fouty , Fifty , Sixty , Seventy , Eighty , Ninety , Hundred , Thousand]

wordNumber n = case n of
	One   -> "one"
	Two   -> "two"
	Three -> "three"
	Four  -> "four" 
	Five -> "Five"
 	Six -> "Six"
 	Seven -> "Seven"
 	Eight -> "Eight"
 	Nine -> "Nine"
 	Ten -> "Ten"
 	Eleven -> "Eleven"
 	Twelve -> "Twelve"
 	Thirteen -> "Thirteen"
 	Fourteen -> "Fourteen"
 	Fifteen -> "Fifteen"
 	Sixteen -> "Sixteen"
 	Seventeen -> "Seventeen"
 	Eighteen -> "Eighteen"
 	Nineteen -> "Nineteen"
 	Twenty -> "Twenty"
 	Thirty -> "Thirty"
 	Fouty -> "Fouty"
 	Fifty -> "Fifty"
 	Sixty -> "Sixty"
 	Seventy -> "Seventy"
 	Eighty -> "Eighty"
 	Ninety -> "Ninety"
 	Hundred -> "Hundred"
 	Thousand -> "Thousand"
 	And -> "And"
	

data2word :: NumberWords -> String
data2word [] = []
data2word (x:xs) = (wordNumber x) ++ (data2word xs)

number2data :: Int -> NumberWords
number2data n
  | (n < 20)         = numbers1to19 !! (n - 1) : []
  | (mod n 100 == 0 && n < 1000) = huns : Hundred : []
  | (mod n 10 == 0 && n < 100) = tens : []
  | (n < 100)        = tens : ones : []
  | (n < 1000)       = huns : Hundred : And : (number2data ( mod n 100 ))
  | otherwise        = One : Thousand: []
	where tens = numbersTens  !! ((div n 10) - 1)
	      ones = numbers1to19 !! ((mod n 10) - 1)
	      huns = numbers1to19 !! ((div n 100) - 1)
	      hten = numbersTens  !! ((div (div n 10) 10) -1 )
	      
--wordnumLen :: Int -> Int
wordNumLen n = length $ data2word $ number2data n

p17 = sum $  map (\a -> wordNumLen a) [1 .. 1000]
