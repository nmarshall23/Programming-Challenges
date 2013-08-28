-- You are given the following information, but you may prefer to do some research for yourself.

--  1 Jan 1900 was a Monday.
--  Thirty days has September,
--  April, June and November.
--  All the rest have thirty-one,
--  Saving February alone,
--  Which has twenty-eight, rain or shine.
--  And on leap years, twenty-nine.
--  A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data MonthName = January | February | March | April | May | June | July | August | September | October | November | December
	deriving (Eq, Ord, Enum, Show, Bounded)

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
	deriving (Eq, Ord, Enum, Show, Bounded)

monthnames :: [MonthName]
monthnames = cycle [ January , February , March , April , May , June , July , August , September , October , November , December]

weekdays :: [WeekDay]
weekdays = cycle [ Monday , Tuesday , Wednesday , Thursday , Friday , Saturday , Sunday]

skipNweekDays :: WeekDay -> Int -> WeekDay
skipNweekDays cwd n = head $ drop n $ fromWeekDayN cwd

fromWeekDayN :: WeekDay -> [WeekDay]
fromWeekDayN cwd =  dropWhile(< cwd) weekdays

data Month = 
	Month { name         :: MonthName
	      , numberOfDays :: Int
	      , firstDay     :: WeekDay
	      , year	     :: Year
	      }
	deriving (Eq, Ord, Show)

type Year = Int
type Day  = Int

numberOfDaysInMonth :: MonthName -> Year -> Int
numberOfDaysInMonth m y = case m of
	  September -> 30
	  April	    -> 30
	  June	    -> 30
	  November  -> 30
	  February  -> calFeb y
	  _	    -> 31
	where calFeb a = if isLeapyear a
		then 29
		else 28

isLeapyear :: Year -> Bool
isLeapyear y
  | (y == 1900)        = False -- fix me!!!
  | (y `mod` 4 == 0 )  = True
  | otherwise = False


getYear :: Year -> Year -> WeekDay -> [Month]
getYear sy ey fd = setDay fd $ setyear sy mlist  
	where mlist = take takeXyears $  map (\ m -> Month{ name=m , numberOfDays = 0,firstDay = Monday,year=0}  ) monthnames
	      takeXyears = (ey - sy + 1) * 12
	      setDay _ [] = []
	      setDay cd (m:ms) = m{ firstDay= cd} : setDay dayhelper ms
		where dayhelper = skipNweekDays cd $ numberOfDays m
	      setyear _ [] = [] 
	      setyear y m = (yhelper $ take 12 $ m) ++ (setyear (y + 1) $ drop 12 m)
	      	where yhelper [] = []
	      	      yhelper (x:xs) = x{year=y,numberOfDays=numberOfDaysInMonth (name x) y } : yhelper xs


numberOfSundays :: Int
numberOfSundays = length $ filter isSunday $ drop 12 $ getYear 1900 2000 Monday
	where isSunday m = (firstDay m == Sunday) 
