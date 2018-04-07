module Luhn exposing (valid)

import Char

valid : String -> Bool
valid input =
    let
        cleanInput =
            String.filter ((/=) ' ') input

        multiplyEverySecond i n =
            case rem i 2 of
                1 ->
                    let
                        newN =
                            n * 2
                    in
                        if newN > 9 then
                            newN - 9
                        else
                            newN
            
                _ ->
                    n

    in
        if not (List.all Char.isDigit (String.toList cleanInput)) then
            False
        else if String.length cleanInput < 2 then
            False
        else
            String.toList cleanInput
            |> List.map String.fromChar
            |> List.map (\ s -> Result.withDefault 0 (String.toInt s))
            |> List.reverse
            |> List.indexedMap multiplyEverySecond
            |> List.foldl (+) 0
            |> flip rem 10
            |> (==) 0
