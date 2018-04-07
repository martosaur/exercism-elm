module Say exposing (..)

import Dict

type SayError
    = Negative
    | TooLarge

dozens : Dict.Dict Int String
dozens =
    Dict.empty
    |> Dict.insert 2 "twenty"
    |> Dict.insert 3 "thirty"
    |> Dict.insert 4 "forty"
    |> Dict.insert 5 "fifty"
    |> Dict.insert 6 "sixty"
    |> Dict.insert 7 "seventy"
    |> Dict.insert 8 "eighty"
    |> Dict.insert 9 "ninety"

numbers : Dict.Dict Int String
numbers =
    Dict.empty
    |> Dict.insert 1 "one"
    |> Dict.insert 2 "two"
    |> Dict.insert 3 "three"
    |> Dict.insert 4 "four"
    |> Dict.insert 5 "five"
    |> Dict.insert 6 "six"
    |> Dict.insert 7 "seven"
    |> Dict.insert 8 "eight"
    |> Dict.insert 9 "nine"
    |> Dict.insert 10 "ten"
    |> Dict.insert 11 "eleven"
    |> Dict.insert 12 "twelve"
    |> Dict.insert 13 "thirteen"
    |> Dict.insert 14 "fourteen"
    |> Dict.insert 15 "fifteen"
    |> Dict.insert 16 "sixteen"
    |> Dict.insert 17 "seventeen"
    |> Dict.insert 18 "eighteen"
    |> Dict.insert 19 "nineteen"


say : Int -> Result SayError String
say number =
    let
        dropLeadingAnd s =
            if String.startsWith "and " s then
                String.dropLeft 4 s
            else
                s

        actuallySay (billions, millions, thousands, rest) =
            [ sayBillions billions
            , sayMillions millions
            , sayThousands thousands
            , (if rest > 99 || rest == 0 then sayHundreds rest else "and " ++ (saySmallNumber rest))]
            |> List.filter ((/=) "")
            |> String.join(" ")
            |> String.trim
            |> dropLeadingAnd
    in
        
    if number < 0 then
        Err Negative
    else if number > 999999999999 then
        Err TooLarge
    else if number == 0 then
        Ok "zero"
    else
        split number
        |> actuallySay
        |> Ok


getString : Int -> Dict.Dict Int String -> String
getString number dict =
    Maybe.withDefault "" (Dict.get number dict)


split : Int -> (Int, Int, Int, Int)
split number =
    let
        billions =
            number // 1000000000

        millions =
            rem (number // 1000000) 1000

        thousands =
            rem (number // 1000) 1000

        hundreds =
            rem (number // 100) 10

        rest =
            rem number 1000
    in
        (billions, millions, thousands, rest)


saySmallNumber : Int -> String
saySmallNumber number =
    if number < 20 then
        getString number numbers
    else if rem number 10 == 0 then
        getString (number // 10) dozens
    else
        (getString (number // 10) dozens)
        ++
        "-"
        ++
        (getString (rem number 10) numbers)


sayHundreds : Int -> String
sayHundreds number =
    let
        hundreds =
            number // 100
        
        remaining =
            rem number 100
    in
        if hundreds == 0 then
            saySmallNumber number
        else if remaining == 0 then
            (saySmallNumber hundreds) ++ " hundred"
        else
            (saySmallNumber hundreds) ++ " hundred and " ++ (saySmallNumber remaining)


sayBillions : number -> String
sayBillions number =
    if number == 0 then
        ""
    else
        (sayHundreds number) ++ " billion"


sayMillions : number -> String
sayMillions number =
    if number == 0 then
        ""
    else
        (sayHundreds number) ++ " million"


sayThousands : number -> String
sayThousands number =
    if number == 0 then
        ""
    else
        (sayHundreds number) ++ " thousand"