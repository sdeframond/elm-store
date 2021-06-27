module Id exposing
    ( Id
    , fromInt
    , next
    , one
    , toInt
    )


type Id a
    = Id Int


one : Id a
one =
    Id 1


next : Id a -> Id a
next (Id i) =
    Id (i + 1)


toInt : Id a -> Int
toInt (Id i) =
    i


fromInt : Int -> Id a
fromInt i =
    Id i
