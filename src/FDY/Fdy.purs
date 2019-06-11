module FDY where

import Prelude

foreign import remainder :: Int -> Int -> Int

infixl 9 remainder as %

data Day = SUN | MON | TUE | WED | THUR | FRI | SAT

instance genShowForDay :: Show Day where
    show SUN = "Sunday"
    show MON = "Monday"
    show TUE = "Tuesday"
    show WED = "Wednesday"
    show THUR = "Thursday"
    show FRI = "Friday"
    show SAT = "Saturday"

doomDays :: Int -> Int -> Int
doomDays 5 _ = 9
doomDays 9 _ = 5
doomDays 7 _ = 11
doomDays 11 _ = 7
doomDays 1 y = handleLeapYear 3 y
doomDays 2 y= handleLeapYear 28 y
doomDays mnth _ = mnth

numToDay :: Int -> Day
numToDay 0 = SUN
numToDay 1 = MON
numToDay 2 = TUE
numToDay 3 = WED
numToDay 4 = THUR
numToDay 5 = FRI
numToDay _ = SAT

findDoomDay :: Int -> Int ->  Int
findDoomDay cenCode lastTwo  =
    let secF = lastTwo % 12
        thirF = lastTwo / 12
        fouF = secF/4
    in (cenCode + secF + thirF + fouF ) % 7

getCenturyCode :: Int -> Int
getCenturyCode 1800 = 5
getCenturyCode 1900 = 3
getCenturyCode 2000 = 2
getCenturyCode 2100 = 0
getCenturyCode x = let diff = (x - 1800)/10
                    in find (if diff < 31 && diff > -31 then diff/10  else diff % 4)

find ::  Int -> Int
find val
            | val == 0 = getCenturyCode 1800
            | val == 1 = getCenturyCode 1900
            | val == 2 = getCenturyCode 2000
            | otherwise = getCenturyCode 2100

handleLeapYear :: Int -> Int -> Int
handleLeapYear day year
    | (isLeapYear year) = day + 1
    | otherwise = day

isLeapYear :: Int -> Boolean
isLeapYear year = if year % 100 == 0 && year % 4 == 0 then true else false

getMeDay :: Int -> Int -> Int -> Day
getMeDay date mon year =
    let cenCode = getCenturyCode year
        doomVal = findDoomDay cenCode (year % 100)
        doomDay = doomDays mon year
        diffVal = if date < doomDay then doomDay + doomVal - date else doomVal + date - doomDay
     in numToDay (diffVal % 7)
