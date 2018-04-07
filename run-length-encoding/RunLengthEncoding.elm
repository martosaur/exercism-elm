module RunLengthEncoding exposing (decode, encode, version)

import Debug exposing (log)
import Char

version =
    2

encode : String -> String
encode string =
    doEncode "" 0 '\0' string

doEncode : String -> Int -> Char -> String -> String
doEncode acc currentCount currentChar string =
    case String.uncons string of
        Nothing ->
            appendPair currentCount currentChar acc

        Just (c, rest) ->
            if currentCount == 0 then
                doEncode acc 1 c rest
            else if c == currentChar then
                doEncode acc (currentCount + 1) currentChar rest
            else
                doEncode (appendPair currentCount currentChar acc) 1 c rest

appendPair : Int -> Char -> String -> String
appendPair n char string =
    if n == 1 then
        string ++ (String.fromChar char)
    else
       string ++ (toString n) ++ (String.fromChar char) 


decode : String -> String
decode string =
    doDecode "" "" string

doDecode : String -> String -> String -> String
doDecode acc currentNumber string =
    case String.uncons string of
        Nothing ->
            acc
    
        Just (c, rest) ->
            if Char.isDigit c then
                doDecode acc (currentNumber ++ (String.fromChar c)) rest
            else
                doDecode (appendNChars currentNumber c acc) "" rest

appendNChars : String -> Char -> String -> String
appendNChars n char string =
    let
        number = Result.withDefault 1 (String.toInt n)
    in
        string ++ (String.repeat number (String.fromChar char))