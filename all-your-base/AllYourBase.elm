module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    let
        decimal =
            toDecimal inBase digits 0
    in  
        if List.all ( (==) 0) digits  then
            Nothing
        else if inBase <= 1 || outBase <= 1 then
            Nothing
        else if List.any (flip (<) 0) digits then
            Nothing
        else if List.any (flip (>=) inBase) digits then
            Nothing
        else
            Just (fromDecimal outBase decimal [])


toDecimal : Int -> List Int -> Int -> Int
toDecimal inBase digits acc =
    let
        power =
            (List.length digits) - 1
    in
        case digits of
            h::rest ->
                toDecimal inBase rest (acc + h * inBase ^ power)
        
            [] ->
                acc


fromDecimal : Int -> Int -> List Int -> List Int
fromDecimal toBase number acc =
    let
        power =
            List.length acc

        nextDigit =
            number // (toBase ^ power)
            |> flip rem toBase
    in
        case number of
            0 ->
                acc
        
            _ ->
                fromDecimal toBase (number - nextDigit * toBase ^ power) (nextDigit::acc)