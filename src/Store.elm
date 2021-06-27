module Store exposing
    ( Store, System
    , Index, initConfig, withIndex, makeSystem
    )

{-| A simple store with support for secondary indexes.


# Main types

@docs Store, System


# Indices Configuration

@docs Index, initConfig, withIndex, makeSystem

-}

import Dict exposing (Dict)
import Id exposing (Id)
import IdDict exposing (IdDict)


{-| A store. Basically like a `Dict (Id a) a` but that can be manipulated with
the functions in `System` in order to support indices.

This should appear in your model.

-}
type Store a
    = Store
        { data : IdDict a
        , nextId : Id a
        , indices : IdDict (IndexStore a)
        }


type Config a indices
    = Config
        { indices : indices
        , insert : Id a -> a -> Store a -> Maybe (Store a)
        , nextIndexId : Id (IndexStore a)
        , remove : Id a -> Store a -> Store a
        }


nextIndexId : Config a indices -> ( Config a indices, Id (IndexStore a) )
nextIndexId (Config config) =
    ( Config { config | nextIndexId = Id.next config.nextIndexId }
    , config.nextIndexId
    )


configIndices : Config a indices -> indices
configIndices (Config config) =
    config.indices


{-| An index on stored values. For example if your store contains `MyValue` items, you could define
your indices like this:

    type alias MyIndices =
        { name : Index MyValue
        , anyArbitraryIndexName : Index MyValue
        }

The record constructor `MyIndices` would then be passed to `initConfig`.

-}
type Index a
    = Index (Id (IndexStore a))


type IndexStore a
    = IndexStore (Dict String (Id a))


idsEmpty : IndexStore a
idsEmpty =
    IndexStore Dict.empty


idsGet : String -> IndexStore a -> Maybe (Id a)
idsGet key (IndexStore d) =
    Dict.get key d


idsInsert : String -> Id a -> IndexStore a -> Maybe (IndexStore a)
idsInsert key id ids =
    let
        insert_ (IndexStore d) =
            IndexStore (Dict.insert key id d)
    in
    case idsGet key ids of
        Just existingId ->
            if id == existingId then
                Just ids

            else
                Nothing

        Nothing ->
            Just (insert_ ids)


idsRemove : String -> IndexStore a -> IndexStore a
idsRemove key (IndexStore d) =
    Dict.remove key d |> IndexStore


{-| A set of functions to manipulate a `Store`.

This should NOT be part of your model.

You can generate one with `makeSystem`

The available functions are :

  - `initStore` : creates an empty store.
  - `get` : gets an item by its Id.
  - `create` : creates a new item in the store and returns its `Id`. Fails if the new item breaks
    unicity constraints.
  - `insert` : insert an item at a given `id`. Fails if the new item breaks
    unicity constraints.
  - `toList` : returns all items in a store as a list of `(id, item)`.
  - `getBy` : gets an item by a secondary key.
    Example : `system.getBy .lowerCaseName "john doe" store`
  - `getIdBy` : gets an item's `Id` by a secondary key.
  - `remove` : deletes an item from the store.

-}
type alias System a indices =
    { initStore : Store a
    , get : Id a -> Store a -> Maybe a
    , create : a -> Store a -> Maybe ( Id a, Store a )
    , insert : Id a -> a -> Store a -> Maybe (Store a)
    , toList : Store a -> List ( Id a, a )
    , getBy : (indices -> Index a) -> String -> Store a -> Maybe a
    , getIdBy : (indices -> Index a) -> String -> Store a -> Maybe (Id a)
    , remove : Id a -> Store a -> Store a
    }


{-| Makes a system from a given config.

    Store.initConfig MyIndices
        |> Store.withIndex .name
        |> Store.withIndex (.age >> String.fromInt)
        |> Store.makeSystem

-}
makeSystem : Config a indices -> System a indices
makeSystem ((Config configData) as config) =
    { initStore = initStore config
    , get = get
    , create = create config
    , insert = configData.insert
    , toList = toList
    , getBy = getBy (configIndices config)
    , getIdBy = getIdBy (configIndices config)
    , remove = configData.remove
    }


{-| Creates a new system configuration.

Typically `indices` should be a record constructor so that each index can be
configured using `withIndex`.

-}
initConfig : indices -> Config a indices
initConfig builder =
    { indices = builder
    , nextIndexId = Id.one
    , insert = insert
    , remove = remove
    }
        |> Config


{-| Configures an index on the stored items.

This index adds a unicity constraint : no two items can have the same indexed key.

Example :

    -- Given `type alias MyIndices = { lowerCaseName : Index MyValue }`
    initConfig MyIndices
        |> withIndex (.lowerCaseName >> String.toLower)

-}
withIndex : (a -> String) -> Config a (Index a -> indices) -> Config a indices
withIndex toKey config_ =
    let
        ( Config config, indexId ) =
            nextIndexId config_

        getIndex key store =
            IdDict.get key store.indices |> Maybe.withDefault idsEmpty

        setIndex key ids store =
            { store | indices = IdDict.insert key ids store.indices }

        insert_ : Id a -> a -> Store a -> Maybe (Store a)
        insert_ id item (Store store) =
            getIndex indexId store
                |> idsInsert (toKey item) id
                |> Maybe.map (\ids -> setIndex indexId ids store)
                |> Maybe.map Store
                |> Maybe.andThen (config.insert id item)

        remove_ id store =
            get id store
                |> Maybe.map (removeItemFromIndex store)
                |> Maybe.withDefault store
                |> config.remove id

        removeItemFromIndex (Store store) item =
            getIndex indexId store
                |> idsRemove (toKey item)
                |> (\ids -> setIndex indexId ids store)
                |> Store
    in
    { indices = config.indices (Index indexId)
    , insert = insert_
    , nextIndexId = config.nextIndexId
    , remove = remove_
    }
        |> Config


nextId : Store a -> ( Id a, Store a )
nextId (Store store) =
    { store | nextId = Id.next store.nextId }
        |> Store
        |> Tuple.pair store.nextId


initStore : Config a indices -> Store a
initStore (Config config) =
    Store
        { data = IdDict.empty
        , nextId = Id.one
        , indices = IdDict.empty
        }


get : Id a -> Store a -> Maybe a
get id (Store store) =
    IdDict.get id store.data


create : Config a indices -> a -> Store a -> Maybe ( Id a, Store a )
create (Config config) item store =
    let
        ( id, nextIdStore ) =
            nextId store
    in
    config.insert id item nextIdStore
        |> Maybe.map (Tuple.pair id)


insert : Id a -> a -> Store a -> Maybe (Store a)
insert id item (Store store) =
    { store | data = IdDict.insert id item store.data }
        |> Store
        |> Just


toList : Store a -> List ( Id a, a )
toList (Store store) =
    IdDict.toList store.data


getBy : indices -> (indices -> Index a) -> String -> Store a -> Maybe a
getBy indices getIndex key store =
    getIdBy indices getIndex key store
        |> Maybe.andThen (\id -> get id store)


getIdBy : indices -> (indices -> Index a) -> String -> Store a -> Maybe (Id a)
getIdBy indices getIndex key (Store store) =
    getIndex indices
        |> (\(Index idxId) -> IdDict.get idxId store.indices)
        |> Maybe.andThen (idsGet key)


remove : Id a -> Store a -> Store a
remove id (Store store) =
    { store | data = IdDict.remove id store.data }
        |> Store
