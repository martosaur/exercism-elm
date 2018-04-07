module Pangram exposing (isPangram)

import Char

isPangram : String -> Bool
isPangram sentence =
    let
        chars = 
            String.toList (String.toLower sentence)
        
        abc =
            List.map Char.fromCode (List.range 97 122)
    in
        List.all (\ c -> List.member c chars) abc
