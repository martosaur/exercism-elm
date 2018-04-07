module NucleotideCount exposing (nucleotideCounts, version)


version : Int
version =
    2


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    let
        emptyResult = NucleotideCounts 0 0 0 0
    in
        sequence
        |> String.toList
        |> List.foldl updateNucleotideCounts emptyResult

updateNucleotideCounts : Char -> NucleotideCounts-> NucleotideCounts
updateNucleotideCounts nucleotide nucleotideCounts =
    case nucleotide of
        'A' ->
            {nucleotideCounts | a = nucleotideCounts.a + 1}
        
        'T' ->
            {nucleotideCounts | t = nucleotideCounts.t + 1}
        
        'C' ->
            {nucleotideCounts | c = nucleotideCounts.c + 1}
        
        'G' ->
            {nucleotideCounts | g = nucleotideCounts.g + 1}

        _ ->
            nucleotideCounts
