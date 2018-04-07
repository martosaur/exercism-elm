module BinarySearch exposing (find)

import Array exposing (Array)


find : Int -> Array Int -> Int
find target xs =
    doFind -1 target xs


doFind : Int -> comparable -> Array comparable -> Int
doFind i target xs =
    let
        middleIndex =
            ((Array.length xs) - 1) // 2


        compare target el =
            if target == el then
                i + 1 + middleIndex
            else if target < el then
                doFind i target (Array.slice 0 middleIndex xs)
            else
                doFind (i + 1 + middleIndex) target (Array.slice (middleIndex + 1) (Array.length xs) xs)

    in
        case Array.get middleIndex xs of

            Just el ->
                compare target el

            Nothing ->
                -1
