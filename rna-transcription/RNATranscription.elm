module RNATranscription exposing (toRNA)

import Dict

toRNA : String -> Result Char String
toRNA dna =
    let
        mapping =
            Dict.empty
            |> Dict.insert 'G' 'C'
            |> Dict.insert 'C' 'G'
            |> Dict.insert 'T' 'A'
            |> Dict.insert 'A' 'U'

        rna =
            String.map (\ c -> Maybe.withDefault c (Dict.get c mapping)) dna
    in
        Ok rna