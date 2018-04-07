module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    if isDivisible year 400 then
        True
    else if isDivisible year 100 then
        False
    else if isDivisible year 4 then
        True
    else
        False

isDivisible : Int -> Int -> Bool
isDivisible i d =
    rem i d == 0