module Triangle exposing (Triangle(..), triangleKind, validateSides, version)

import Set

type Triangle
    = Equilateral
    | Isosceles
    | Scalene


version : Int
version =
    2


triangleKind : comparable -> comparable -> comparable -> Result String Triangle
triangleKind x y z =
    validateSides x y z
    |> Result.andThen
        (\ (x, y, z) ->
            case Set.size (Set.fromList [x, y, z]) of
                1 ->
                    Ok Equilateral
            
                2 ->
                    Ok Isosceles

                3 ->
                    Ok Scalene

                _ ->
                    Err "Unknown Error"
        )

validateSides : number -> number -> number -> Result String (comparable, comparable, comparable)
validateSides x y z =
    if List.any (\ i -> i <= 0) [x, y, z] then
        Err "Invalid lengths"
    else if (x + y) < z || (x + z) < y || (y + z) < x then
        Err "Violates inequality"
    else
        Ok (x, y, z)