module Transpose exposing (transpose)


transpose : List String -> List String
transpose lines =
    doLines [] lines

doLines : List String -> List String -> List String
doLines acc lines =
    if List.all String.isEmpty lines then
        acc
    else
        let
            trimCount =
                List.foldl (\ s acc -> if String.isEmpty s then acc + 1 else 0) 0 lines

            newRow =
                List.map (String.left 1) lines
                |> List.map (\ s -> if s == "" then " " else s)
                |> List.foldr (++) ""
                |> String.dropRight trimCount
        in
            doLines (acc ++ [newRow]) (List.map (String.dropLeft 1) lines)