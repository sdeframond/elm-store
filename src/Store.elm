module Store exposing
    ( Index
    , Store
    , System
    , initConfig
    , makeSystem
    , withIndex
    )

import Dict exposing (Dict)
import Id exposing (Id)
import IdDict exposing (IdDict)


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


initConfig : indices -> Config a indices
initConfig builder =
    { indices = builder
    , nextIndexId = Id.one
    , insert = insert
    , remove = remove
    }
        |> Config


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



-- withIndex : (a -> String) -> (newIndices -> Index) -> Config a indices -> Config a newIndices
-- withIndex toKey index system =
--     Config
--         system.initStore
--         system.get
--         system.create
--         system.insert
--         system.remove
--         system.update
--         system.indices
