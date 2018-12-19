module FDY where


import Prelude (otherwise, (&&), (*), (+), (-), (/), (==))


foreign import remainder :: Int -> Int -> Int 

infixl 9 remainder as %

data Day = SUN | MON | TUE | WED | THUR | FRI | SAT


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


findDoomDay :: Int -> Int -> Int -> Int 
findDoomDay cenCode lastTwo leapVal =
    let secF = lastTwo % 12
        thirF = lastTwo / 12
        fouF = thirF % secF
    in (secF + thirF + fouF + leapVal) % 7 


getCenturyCode :: Int -> Int
getCenturyCode 1800 = 5
getCenturyCode 1900 = 3
getCenturyCode 2000 = 2
getCenturyCode 2100 = 0
getCenturyCode x = find (((x - 1800)/100) % 4)
    where
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
        doomVal = findDoomDay cenCode (year % 10 + (year % 100)*10) (if isLeapYear year then 2 else 0)
        diffVal = doomVal + date - (doomDays mon year)
     in numToDay (diffVal % 7)
