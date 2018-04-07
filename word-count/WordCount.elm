module WordCount exposing (wordCount)

import Dict
import Char

wordCount : String -> Dict.Dict String Int
wordCount sentence =
    sentence
    |> String.toLower
    |> String.words
    |> List.map (\ word -> 
                    String.filter (\ c -> Char.isDigit c || Char.isLower c) word)
    |> List.filter (String.isEmpty >> not)
    |> doCount Dict.empty

doCount : Dict.Dict comparable Int -> List comparable -> Dict.Dict comparable Int
doCount acc list =
    case list of
        [] ->
            acc
    
        h::rest ->
            let
                current_count =
                    acc
                    |> Dict.get h
                    |> Maybe.withDefault 0

                new_acc =
                    Dict.insert h (current_count + 1) acc
            in
                doCount new_acc rest
