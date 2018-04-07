module ScrabbleScore exposing (scoreWord)

import Dict

scoreWord : String -> Int
scoreWord x =
    let
        scoreboard =
            Dict.empty
            |> Dict.insert 'A' 1
            |> Dict.insert 'E' 1
            |> Dict.insert 'I' 1
            |> Dict.insert 'O' 1
            |> Dict.insert 'U' 1
            |> Dict.insert 'L' 1
            |> Dict.insert 'N' 1
            |> Dict.insert 'R' 1
            |> Dict.insert 'S' 1
            |> Dict.insert 'T' 1
            |> Dict.insert 'D' 2
            |> Dict.insert 'G' 2
            |> Dict.insert 'B' 3
            |> Dict.insert 'C' 3
            |> Dict.insert 'M' 3
            |> Dict.insert 'P' 3
            |> Dict.insert 'F' 4
            |> Dict.insert 'H' 4
            |> Dict.insert 'V' 4
            |> Dict.insert 'W' 4
            |> Dict.insert 'Y' 4
            |> Dict.insert 'K' 5
            |> Dict.insert 'J' 8
            |> Dict.insert 'X' 8
            |> Dict.insert 'Q' 10
            |> Dict.insert 'Z' 10
            
    in
        String.toUpper x
        |> String.toList
        |> List.map (\ c -> Dict.get c scoreboard |> Maybe.withDefault 0)
        |> List.sum
