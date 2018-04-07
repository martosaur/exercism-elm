module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    doKeep predicate [] list

doKeep : (a -> Bool) -> List a -> List a -> List a
doKeep predicate acc list =
    case list of
        h::rest ->
            if predicate h then
                doKeep predicate (acc ++ [h]) rest
            else
                doKeep predicate acc rest
    
        [] ->
            acc
            

discard : (a -> Bool) -> List a -> List a
discard predicate list =
    keep (predicate >> not) list
