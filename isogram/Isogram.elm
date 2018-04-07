module Isogram exposing (isIsogram)

import Set

isIsogram : String -> Bool
isIsogram sentence =
    let
        cleanedSentence =
            String.filter (\ c -> not (c == '-' || c == ' ')) sentence
            |> String.toLower

        charSet =
            String.toList cleanedSentence
            |> Set.fromList
    
    in
        String.length cleanedSentence == Set.size charSet