module Allergies exposing (isAllergicTo, toList)

import Bitwise
import Dict

allergenes =
    Dict.empty
    |> Dict.insert "eggs" 1
    |> Dict.insert "peanuts" 2
    |> Dict.insert "shellfish" 4
    |> Dict.insert "strawberries" 8
    |> Dict.insert "tomatoes" 16
    |> Dict.insert "chocolate" 32
    |> Dict.insert "pollen" 64
    |> Dict.insert "cats" 128

isAllergicTo : String -> Int -> Bool
isAllergicTo name score =
    Dict.get name allergenes
    |> Maybe.withDefault -1
    |> Bitwise.and score
    |> (/=) 0

toList : Int -> List String
toList score =
    Dict.keys allergenes
    |> List.filter (flip isAllergicTo score)
