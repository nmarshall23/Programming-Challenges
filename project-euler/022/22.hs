-- Using names.txt (right click and 'Save Link/Target As...'),
-- a 46K text file containing over five-thousand first names,
-- begin by sorting it into alphabetical order. 
-- Then working out the alphabetical value for each name, 
-- multiply this value by its alphabetical position in the list
-- to obtain a name score.

-- For example, when the list is sorted into alphabetical order, 
-- COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name 
-- in the list. So, COLIN would obtain a score of 938 * 53 = 49714.

-- What is the total of all the name scores in the file?

import List (sort)
import IO
import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Char


alphaValues [] = []
alphaValues (x:xs) = val x:alphaValues xs
	where val a = length $ takeWhile ( <= a) ['A' .. 'Z']


nameScore _ [] = []
nameScore m (x:xs) = ((nameSum x) * m) : nameScore (m+1) xs
	where nameSum n  = sum $ alphaValues n

nameScoreTotal ns = sum $ nameScore 1 $ sort ns

--main = do x <- openFile "names.txt" ReadMode
--	  y <- hGetLine x

-- take list of names -> sort -> find name score -> get total
main = do
	result <- parseFromFile aName "names.txt"
	case (result) of
                Left err  -> print err
                Right xs  -> print (nameScoreTotal' xs)
                --Right xs  -> print (nameScoreTotal xs)


aName :: Parser [String]
aName = do
	 ddq
	 words <- sepEndBy1 word separator
	 return words

word    :: Parser String
word    = many1 letter

separator   :: Parser ()
separator   = skipMany1 ( oneOf "\\\"," )
--separator   = skipMany ( comma <|> ddq )

ddq   = string "\""


-- alt solution one funtion scorer
nameScoreTotal' x = sum . zipWith (*) [1..] . 
    map (sum . map ((subtract 64) . ord)) $ sort x

