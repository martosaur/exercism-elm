module Etl exposing (transform)

import Dict

transform : Dict.Dict Int (List String) -> Dict.Dict String Int
transform input =
    Dict.foldl pushToMap Dict.empty input

pushToMap : Int -> List String -> Dict.Dict String Int -> Dict.Dict String Int
pushToMap score list acc =
    let
        insertScore s acc =
            Dict.insert (String.toLower s) score acc

    in
        List.foldl insertScore acc list
                