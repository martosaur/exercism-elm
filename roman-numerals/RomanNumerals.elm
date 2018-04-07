module RomanNumerals exposing (..)


toRoman : Int -> String
toRoman number =
    let
        thousands =
            number // 1000

        hundreds =
            rem (number // 100) 1000

        dozens =
            rem (number // 10) 10

        rest =
            rem number 10 
    in
        (romanThousands thousands)
        ++ (romanHundreds hundreds)
        ++ (romanDozens dozens)
        ++ (romanDigits rest)


romanThousands : number -> String
romanThousands number =
    case number of
        1 ->
            "M"
    
        2 ->
            "MM"

        3 ->
            "MMM"

        _ ->
            ""

romanHundreds : number -> String
romanHundreds number =
    case number of
        1 ->
            "C"
    
        2 ->
            "CC"

        3 ->
            "CCC"

        4 ->
            "CD"

        5 ->
            "D"

        6 ->
            "DC"

        7 ->
            "DCC"

        8 ->
            "DCCC"

        9 ->
            "CM"
        
        _ ->
            ""


romanDozens : number -> String
romanDozens number =
    case number of
        1 ->
            "X"
    
        2 ->
            "XX"

        3 ->
            "XXX"

        4 ->
            "XL"

        5 ->
            "L"

        6 ->
            "LX"

        7 ->
            "LXX"

        8 ->
            "LXXX"
        
        9 ->
            "XC"
        
        _ ->
            ""


romanDigits : number -> String
romanDigits number =
    case number of
        1 ->
            "I"
    
        2 ->
            "II"

        3 ->
            "III"

        4 ->
            "IV"

        5 ->
            "V"

        6 ->
            "VI"

        7 ->
            "VII"

        8 ->
            "VIII"
        
        9 ->
            "IX"
        
        _ ->
            ""