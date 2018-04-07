module AtbashCipher exposing (decode, encode)

import Dict
import Debug

key =
    Dict.empty
    |> Dict.insert 'a' 'z'
    |> Dict.insert 'b' 'y'
    |> Dict.insert 'c' 'x'
    |> Dict.insert 'd' 'w'
    |> Dict.insert 'e' 'v'
    |> Dict.insert 'f' 'u'
    |> Dict.insert 'g' 't'
    |> Dict.insert 'h' 's'
    |> Dict.insert 'i' 'r'
    |> Dict.insert 'j' 'q'
    |> Dict.insert 'k' 'p'
    |> Dict.insert 'l' 'o'
    |> Dict.insert 'm' 'n'
    |> Dict.insert 'n' 'm'
    |> Dict.insert 'o' 'l'
    |> Dict.insert 'p' 'k'
    |> Dict.insert 'q' 'j'
    |> Dict.insert 'r' 'i'
    |> Dict.insert 's' 'h'
    |> Dict.insert 't' 'g'
    |> Dict.insert 'u' 'f'
    |> Dict.insert 'v' 'e'
    |> Dict.insert 'w' 'd'
    |> Dict.insert 'x' 'c'
    |> Dict.insert 'y' 'b'
    |> Dict.insert 'z' 'a'
    |> Dict.insert '0' '0'
    |> Dict.insert '1' '1'
    |> Dict.insert '2' '2'
    |> Dict.insert '3' '3'
    |> Dict.insert '4' '4'
    |> Dict.insert '5' '5'
    |> Dict.insert '6' '6'
    |> Dict.insert '7' '7'
    |> Dict.insert '8' '8'
    |> Dict.insert '9' '9'

encode : String -> String
encode plain =
    let
        cleanedPlain =
            String.toLower plain
    in
        String.map (\ c -> Maybe.withDefault '\0' (Dict.get c key)) cleanedPlain
        |> String.filter ((/=) '\0')
        |> split5

split5 : String -> String
split5 s =
    let
        split el acc =
            case rem (String.length acc) 6 of
                5 ->
                    acc ++ " " ++ (String.fromChar el)
            
                _ ->
                    acc ++ (String.fromChar el)
    in
        String.foldl split "" s

decode : String -> String
decode cipher =
    encode cipher
    |> String.split " "
    |> String.join ""
