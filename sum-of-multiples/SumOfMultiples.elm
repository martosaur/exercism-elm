module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    List.range 1 (limit - 1)
    |> List.filter (\ n ->
                        List.any (\ m -> n % m == 0) multiples)
    |> List.sum
