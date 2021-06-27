module IdDict exposing
    ( IdDict
    , empty
    , get
    , insert
    , remove
    , toList
    )

import Dict exposing (Dict)
import Id exposing (Id)


type IdDict a
    = IdDict (Dict Int a)


empty : IdDict a
empty =
    IdDict <| Dict.empty


get : Id a -> IdDict a -> Maybe a
get id (IdDict d) =
    Dict.get (Id.toInt id) d


insert : Id a -> a -> IdDict a -> IdDict a
insert id value (IdDict d) =
    Dict.insert (Id.toInt id) value d |> IdDict


toList : IdDict a -> List ( Id a, a )
toList (IdDict d) =
    Dict.toList d |> List.map (Tuple.mapFirst Id.fromInt)


remove : Id a -> IdDict a -> IdDict a
remove id (IdDict d) =
    Dict.remove (Id.toInt id) d |> IdDict
