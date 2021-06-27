module Id exposing (Id, one, next, equalsInt, toInt, fromInt)

{-|

@docs Id, one, next, equalsInt, toInt, fromInt

-}


{-| An Id for a given value.

Technically just a wrapper around `Int` with a phantom type parameter.

-}
type Id a
    = Id Int


{-| The first possible `Id`.
-}
one : Id a
one =
    Id 1


{-| Returns the next `Id`.
-}
next : Id a -> Id a
next (Id i) =
    Id (i + 1)


{-| Unwrap the inner `Int` value.
-}
toInt : Id a -> Int
toInt (Id i) =
    i


{-| Checks equality between an `Int` and an `Id`'s inner value.
-}
equalsInt : Int -> Id a -> Bool
equalsInt i (Id j) =
    i == j


{-| Wraps an Int into an Id.

Use with caution. In most cases, `one`, `next` and `equalsInt` are enough.

A possible use case would be to deserialize ids from JSON.

-}
fromInt : Int -> Id a
fromInt i =
    Id i
