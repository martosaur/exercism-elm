module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if size == 0 then
        Err "Invalid size: 0"
    else
        input
        |> String.toList
        |> doConvert []
        |> Result.andThen (\ list -> Ok (doSlice [] size list))

doSlice : (List (List Int)) -> Int -> List Int -> (List (List Int))
doSlice acc size input =
    if List.length input < size then
        acc
    else
        doSlice (acc ++ [(List.take size input)]) size (List.drop 1 input)

doConvert : List Int -> List Char -> Result String (List Int)
doConvert acc input =
    case input of
        [] ->
            Ok acc
    
        head::rest ->
            let
                s = String.fromChar head
            in
                case String.toInt s of
                    Ok n ->
                        doConvert (acc ++ [n]) rest
                
                    Err _ ->
                        Err ("could not convert string \'" ++ s ++ "\' to an Int")