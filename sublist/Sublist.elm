module Sublist exposing (ListComparison(..), sublist, version)


version : Int
version =
    2


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    if alist == blist then
        Equal
    else if isSublist alist blist then
        Sublist
    else if isSublist blist alist then
        Superlist
    else
        Unequal

isSublist : List a -> List a -> Bool
isSublist a b =
    let
        len_a = List.length a

        len_b = List.length b
    in
        if len_a > len_b then
            False
        else if List.take (len_a) b == a then
            True
        else
            isSublist a (Maybe.withDefault [] (List.tail b))
        