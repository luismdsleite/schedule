module Table exposing (ID(..), Table, add, get, empty, fromList, toList, values, filter)

import Dict exposing (Dict)


type ID
    = ID Int



-- TABLE


type Table info
    = Table Int (Dict Int info)


empty : Table info
empty =
    Table 1 Dict.empty


get : ID -> Table info -> Maybe info
get (ID id) (Table _ dict) =
    Dict.get id dict


add : info -> Table info -> ( Table info, ID )
add info (Table nextId dict) =
    ( Table (nextId + 1) (Dict.insert nextId info dict)
    , ID nextId
    )


{-| Table equivalent of Dict.fromList
I have no idea if this is correct...
-}
fromList : List info -> Table info
fromList list =
    fillTablefromList empty list



fillTablefromList : Table info -> List info -> Table info
fillTablefromList table list =
    case list of
        -- This case is only reached if an empty list is passed
        [] ->
            table

        x :: [] ->
            Tuple.first (add x table)

        x :: xs ->
            fillTablefromList (Tuple.first (add x table)) xs


toList : Table info -> List (Int,info)
toList (Table _ info) = Dict.toList info

values : Table info -> List info
values (Table _ info) = Dict.values info

filter : (Int -> info -> Bool) -> Table info -> List (Int,info)
filter isGood (Table _ info) =  Dict.filter isGood info |> Dict.toList