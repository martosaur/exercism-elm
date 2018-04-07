module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    String.map (\ c -> if c == '-' then ' ' else c) phrase
    |> String.words
    |> List.map (String.left 1)
    |> String.join ""
    |> String.toUpper
