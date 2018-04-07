module ListOps
    exposing
        ( append
        , concat
        , filter
        , foldl
        , foldr
        , length
        , map
        , reverse
        )


length : List a -> Int
length list =
    doLength 0 list


doLength : Int -> List a -> Int
doLength acc list =
    case list of
        [] ->
            acc
    
        _::rest ->
            doLength (acc + 1) rest


reverse : List a -> List a
reverse list =
    doReverse [] list


doReverse : List a -> List a -> List a
doReverse acc list =
    case list of
        [] ->
            acc
    
        head::rest ->
            doReverse (head::acc) rest


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        [] ->
            acc
    
        head::rest ->
            foldl f (f head acc) rest


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    foldl f acc (reverse list)


map : (a -> b) -> List a -> List b
map f list =
    foldl (\ el acc -> acc ++ [(f el)]) [] list


filter : (a -> Bool) -> List a -> List a
filter f list =
    foldl (\ el acc -> if (f el) then acc ++ [el] else acc) [] list


append : List a -> List a -> List a
append xs ys =
    prepend (reverse xs) ys
    |> reverse

prepend : List a -> List a -> List a
prepend acc ys =
    case ys of
        [] ->
            acc
    
        head::rest ->
            prepend (head::acc) rest


concat : List (List a) -> List a
concat list =
    foldl (flip append) [] list
