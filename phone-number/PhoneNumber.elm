module PhoneNumber exposing (getNumber, prettyPrint)

import Char

getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        digits =
            String.filter Char.isDigit phoneNumber

        return digits =
            if String.length digits /= 10 then
                Nothing
            else if String.startsWith "0" digits then
                Nothing
            else if String.slice 3 4 digits == "0" then
                Nothing
            else if String.slice 3 4 digits == "1" then
                Nothing
            else
                Just digits
    in
        case String.uncons digits of
            Nothing ->
                Nothing

            Just ('1', rest) ->
                return rest

            Just _ ->
                return digits
        


prettyPrint : String -> Maybe String
prettyPrint input =
    case getNumber input of
        Nothing ->
            Nothing
    
        Just number ->
            let
                code =
                    "(" ++ (String.left 3 number) ++ ")"

                mid =
                    String.slice 3 6 number

                end =
                    String.right 4 number

            in
                Just (code ++ " " ++ mid ++ "-" ++ end)
            
