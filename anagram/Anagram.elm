module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    List.filter (\ c -> areAnagram word c) candidates

areAnagram : String -> String -> Bool
areAnagram s1 s2 =
    let
        s1Clean = cleanS s1
        s2Clean = cleanS s2
    in
        s1Clean == s2Clean && (String.toLower s1) /= (String.toLower s2)
cleanS : String -> List Char
cleanS s =
    s
    |> String.toLower
    |> String.toList
    |> List.sort