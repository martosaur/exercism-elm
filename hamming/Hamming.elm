module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    let
        a =
            String.toList left
        b =
            String.toList right
        valid = 
            (List.length a) == (List.length b)
    in
        if valid then
            List.map2 (/=) a b
            |> List.filter identity
            |> List.length
            |> Ok
        else
            Err "left and right strands must be of equal length"
                