-- You are given the following information, but you may prefer to do some
-- research for yourself.
--  
-- * 1 Jan 1900 was a Monday.
--  
-- * Thirty days has September, April, June and November.
--  
-- * All the rest have thirty-one, Saving February alone, Which has
--   twenty-eight, rain or shine. And on leap years, twenty-nine.
--  
-- * A leap year occurs on any year evenly divisible by 4, but not on a
--   century unless it is divisible by 400.
--  
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?

import Data.List
import Control.Applicative
import Data.Map

months = [1..12]

numDaysMonth = [31,28,31,30,31,30,31,31,30,31,30,31]

monthDaysMap = fromList $ zipWith (,) months numDaysMonth

days = [1..7]


numDays month year
        | month /= 2 = monthDaysMap ! month
        | otherwise  = if year `mod` 100 == 0 
                        then (if  year `mod` 400 == 0 then 29 else 28)
                        else (if  year `mod` 4 == 0 then 29 else 28)

getFirstDates = unfoldr nextFirst (1,1,1,1900) 

nextFirst (cDay, cDate, cMonth, cYear)
  | cYear > 2000 = Nothing
  | otherwise   = let nDate  = 1
                      nDay   = head $ drop ((numDays cMonth cYear) - 1) $ drop cDay (cycle days)
                      nMonth = if (cMonth + 1) `mod` 12 == 1 then 1 else cMonth + 1
                      nYear  = if nMonth == 1 then (cYear+1) else cYear
                  in Just ((nDay,nMonth,nYear),(nDay,nDate,nMonth,nYear))
                                      
main = do
  print $ length $ Data.List.filter (\(d,m,y) -> d == 7 && y > 1900) getFirstDates                                    
                                                  