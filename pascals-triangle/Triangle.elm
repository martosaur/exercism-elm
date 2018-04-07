module Triangle exposing (rows)


rows : Int -> List (List Int)
rows n =
    List.range 1 n
    |> List.map row


row : number -> List Int
row n =
    case n of
        1 ->
            [1]
    
        2 ->
            [1, 1]

        _ ->
            [1] ++ (shrinkRow (row (n - 1)) [])  ++ [1]


shrinkRow : List Int -> List Int -> List Int
shrinkRow row acc =
    case row of
        a::b::rest ->
            shrinkRow (b::rest) (acc ++ [a + b])
    
        [] ->
            acc

        _ ->
            acc