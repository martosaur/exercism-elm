module Bob exposing (hey)

import Regex


hey : String -> String
hey remark =
    let
        r = extractMeaning remark
    in
        if String.isEmpty r then
            "Fine. Be that way!"
        else if (String.endsWith "?" r) && (allCaps r) then
            "Calm down, I know what I'm doing!" 
        else if String.endsWith "?" r then
            "Sure."
        else if allCaps r then
            "Whoa, chill out!"
        else
            "Whatever."

allCaps : String -> Bool
allCaps remark =
    case Regex.find (Regex.AtMost 1) (Regex.regex "[a-z]+") remark of
        [] -> case Regex.find (Regex.AtMost 1) (Regex.regex "[A-Z]+") remark of
            [] -> False
        
            _ -> True

        _ -> False

extractMeaning : String -> String
extractMeaning remark =
    remark
    |> Regex.find Regex.All (Regex.regex "[a-zA-Z0-9!?]+")
    |> List.map (\ {match} -> match)
    |> String.join ""