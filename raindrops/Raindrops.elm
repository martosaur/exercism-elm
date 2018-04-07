module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let
        s = (has3asFactor number) ++ (has5asFactor number) ++ (has7asFactor number)
    in
        if String.isEmpty s then
            toString number
        else
            s


hasFactor : Int -> String -> Int -> String
hasFactor factor text number =
    case rem number factor of
        0 -> 
            text
        _ ->
            ""

has3asFactor = hasFactor 3 "Pling"
has5asFactor = hasFactor 5 "Plang"
has7asFactor = hasFactor 7 "Plong"