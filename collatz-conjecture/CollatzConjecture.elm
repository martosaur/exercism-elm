module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    doCollatz 0 start

doCollatz : number -> Int -> Result String number
doCollatz steps n =
    if n == 1 then
        Ok steps
    else if n == 0 then
        Err "Only positive numbers are allowed"
    else
        case rem n 2 of
            0 ->
                doCollatz (steps + 1) (n // 2)
        
            1 ->
                doCollatz (steps + 1) (n * 3 + 1)

            _ -> Err "Only positive numbers are allowed"