module Store exposing
    ( Store
    , initStore
    , initSystem
    , withConstraint
    , withIndex
    , withUniqueIndex
    )

import Dict exposing (Dict, insert)
import PositiveInt exposing (PositiveInt)
import Set exposing (Set)


type Store a
    = Store
        { data : Dict String a
        , indices : Dict String (Dict String (Set String))
        , nextId : Id a
        }


type Id a
    = Id PositiveInt


idToString : Id a -> String
idToString (Id id) =
    PositiveInt.toString id


idNext : Id a -> Id a
idNext (Id id) =
    Id (PositiveInt.next id)


initStore : Store a
initStore =
    Store
        { data = Dict.empty
        , indices = Dict.empty
        , nextId = Id PositiveInt.one
        }


type alias System index a =
    { get : Id a -> Store a -> Maybe a
    , getListFromIndex : index -> String -> Store a -> List a
    , getFromIndex : index -> String -> Store a -> Maybe a
    , create : a -> Store a -> Maybe ( Id a, Store a )
    , insert : Id a -> a -> Store a -> Maybe (Store a)
    , remove : Id a -> Store a -> Store a
    , update : Id a -> (Maybe a -> Maybe a) -> Store a -> Maybe (Store a)
    , toString : index -> String
    }


initSystem : (index -> String) -> System index a
initSystem toString =
    System
        get
        (getListFromIndex toString)
        (getFromIndex toString)
        (create insert)
        insert
        remove
        (update get remove insert)
        toString


withIndex : index -> (a -> String) -> System index a -> System index a
withIndex indexKey itemToString system =
    let
        insertInIndex id item maybeIndex =
            maybeIndex
                |> Maybe.map
                    (Dict.update
                        (itemToString item)
                        (Maybe.map ((Set.insert <| idToString id) >> Just)
                            >> Maybe.withDefault (Just <| Set.singleton (idToString id))
                        )
                        >> Just
                    )
                |> Maybe.withDefault
                    (Dict.fromList [ ( itemToString item, Set.singleton <| idToString id ) ]
                        |> Just
                    )

        insertInIndices id item (Store store) =
            { store
                | indices = Dict.update (system.toString indexKey) (insertInIndex id item) store.indices
            }
                |> Store
                |> Just

        removeFromSet id set =
            let
                removed =
                    Set.remove (idToString id) set
            in
            if Set.isEmpty removed then
                Nothing

            else
                Just removed

        removeFromIndex id key maybeIndex =
            maybeIndex
                |> Maybe.map
                    (Dict.update key (Maybe.andThen (removeFromSet id)))

        removeFromIndices : Id a -> Store a -> Store a
        removeFromIndices id (Store store) =
            Store
                { store
                    | indices =
                        system.get id (Store store)
                            |> Maybe.map itemToString
                            |> Maybe.map
                                (\key ->
                                    Dict.update (system.toString indexKey)
                                        (removeFromIndex id key)
                                        store.indices
                                )
                            |> Maybe.withDefault store.indices
                }
    in
    beforeInsert insertInIndices system
        |> beforeRemove removeFromIndices


withUniqueIndex : index -> (a -> String) -> System index a -> System index a
withUniqueIndex indexKey itemToString system =
    let
        isUnique item (Store store) =
            Dict.get (system.toString indexKey) store.indices
                |> Maybe.map (not << Dict.member (itemToString item))
                |> Maybe.withDefault True
    in
    system
        |> withIndex indexKey itemToString
        |> withConstraint isUnique


withConstraint : (a -> Store a -> Bool) -> System index a -> System index a
withConstraint validate system =
    let
        check _ item store =
            if validate item store then
                Just store

            else
                Nothing
    in
    beforeInsert check system


beforeInsert : (Id a -> a -> Store a -> Maybe (Store a)) -> System index a -> System index a
beforeInsert before system =
    let
        insert_ id item store =
            before id item store
                |> Maybe.andThen (system.insert id item)
    in
    { system
        | insert = insert_
        , create = create insert_
        , update = update system.get system.remove insert_
    }


beforeRemove : (Id a -> Store a -> Store a) -> System index a -> System index a
beforeRemove before system =
    let
        remove_ id store =
            store
                |> before id
                |> system.remove id
    in
    { system
        | remove = remove_
        , update = update system.get remove_ system.insert
    }


get : Id a -> Store a -> Maybe a
get id (Store { data }) =
    Dict.get (idToString id) data


getIdsFromIndex : (index -> String) -> index -> String -> Store a -> List String
getIdsFromIndex toString indexKey key (Store { indices }) =
    Dict.get (toString indexKey) indices
        |> Maybe.andThen (\index -> Dict.get key index)
        |> Maybe.map Set.toList
        |> Maybe.withDefault []


getListFromIndex : (index -> String) -> index -> String -> Store a -> List a
getListFromIndex toString indexKey key (Store store) =
    getIdsFromIndex toString indexKey key (Store store)
        |> List.filterMap (\id -> Dict.get id store.data)


getFromIndex : (index -> String) -> index -> String -> Store a -> Maybe a
getFromIndex toString indexKey key (Store store) =
    getIdsFromIndex toString indexKey key (Store store)
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id store.data)


create : (Id a -> a -> Store a -> Maybe (Store a)) -> a -> Store a -> Maybe ( Id a, Store a )
create insert_ item (Store store) =
    insert_ store.nextId item (Store store)
        |> Maybe.map (\(Store newStore) -> ( newStore.nextId, Store { newStore | nextId = idNext newStore.nextId } ))


insert : Id a -> a -> Store a -> Maybe (Store a)
insert id item (Store store) =
    { store | data = Dict.insert (idToString id) item store.data }
        |> Store
        |> Just


remove : Id a -> Store a -> Store a
remove id (Store store) =
    { store
        | data = Dict.remove (idToString id) store.data
    }
        |> Store


update :
    (Id a -> Store a -> Maybe a)
    -> (Id a -> Store a -> Store a)
    -> (Id a -> a -> Store a -> Maybe (Store a))
    -> Id a
    -> (Maybe a -> Maybe a)
    -> Store a
    -> Maybe (Store a)
update get_ remove_ insert_ id alter store =
    case alter (get_ id store) of
        Just item ->
            store |> remove_ id |> insert_ id item

        Nothing ->
            Just (remove_ id store)
