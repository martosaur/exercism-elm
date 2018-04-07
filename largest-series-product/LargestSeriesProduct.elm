module LargestSeriesProduct exposing (largestProduct)

largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if String.length series < length then
        Nothing
    else if length < 0 then
        Nothing
    else if length == 0 then
        Just 1
    else
        case String.toInt series of
            Err _ ->
                Nothing
        
            Ok _ ->
                Just (doLargestProduct 0 length series)
                        
            

doLargestProduct : Int -> Int -> String -> Int
doLargestProduct currentMax len series =
    if String.length series < len then
        currentMax
    else
        let
            currentProduct =
                String.left len series
                |> String.toList
                |> List.map (\ d -> Result.withDefault 0 (String.toInt (String.fromChar d)))
                |> List.product

            nextSeries =
                String.dropLeft 1 series
        in
            if currentProduct > currentMax then
                doLargestProduct currentProduct len nextSeries
            else
                doLargestProduct currentMax len nextSeries