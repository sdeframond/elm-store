module StoreTest exposing (suite)

import Expect
import Id
import Store as Store
import Test exposing (..)


type alias MyRecord =
    { name : String, age : Int }


myRecord : MyRecord
myRecord =
    { name = "foo", age = 20 }


myRecord2 : MyRecord
myRecord2 =
    { name = myRecord.name ++ "q", age = myRecord.age + 1 }


type alias MyIndices =
    { name : Store.Index MyRecord
    , age : Store.Index MyRecord
    }


system : Store.System MyRecord MyIndices
system =
    Store.initConfig MyIndices
        |> Store.withIndex .name
        |> Store.withIndex (.age >> String.fromInt)
        |> Store.makeSystem


unwrapJust : Maybe a -> a
unwrapJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            -- This line should never be reached.
            -- Don't do this in production code.
            Debug.todo "should not happen"


suite : Test
suite =
    describe "Store"
        [ createTest
        , insertTest
        , getByTest
        , removeTest
        ]


createTest : Test
createTest =
    describe "create"
        [ test "it returns an Id" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.map Tuple.first
                    |> Expect.equal (Just Id.one)
        , test "it returns a different id when run twice" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.map Tuple.second
                    |> Maybe.andThen (system.create myRecord)
                    |> Maybe.map Tuple.first
                    |> Expect.notEqual (Just Id.one)
        , test "the item can be retreived" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.andThen (\( id, store ) -> system.get id store)
                    |> Expect.equal (Just myRecord)
        , test "it creates two different records" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.andThen (\( _, store ) -> system.create myRecord2 store)
                    |> Maybe.map (\( _, store ) -> system.toList store)
                    |> Maybe.map (List.map Tuple.second)
                    |> Expect.equal (Just [ myRecord, myRecord2 ])
        , test "it won't create two items with the same name" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.andThen (\( _, store ) -> system.create { name = myRecord.name, age = myRecord.age + 1 } store)
                    |> Expect.equal Nothing
        , test "it won't create two items with the same age" <|
            \_ ->
                system.initStore
                    |> system.create myRecord
                    |> Maybe.andThen (\( _, store ) -> system.create { name = myRecord.name ++ "q", age = myRecord.age } store)
                    |> Expect.equal Nothing
        ]


insertTest : Test
insertTest =
    describe "insert"
        [ test "it succeeds" <|
            \_ ->
                system.initStore
                    |> system.insert Id.one myRecord
                    |> Expect.notEqual Nothing
        , test "it changes the store" <|
            \_ ->
                system.initStore
                    |> system.insert Id.one myRecord
                    |> Expect.notEqual (Just system.initStore)
        , test "it is idempotent" <|
            \_ ->
                system.initStore
                    |> system.insert Id.one myRecord
                    |> Maybe.andThen (system.insert Id.one myRecord)
                    |> Expect.equal
                        (system.initStore
                            |> system.insert Id.one myRecord
                        )
        ]


getByTest : Test
getByTest =
    let
        ( id, store ) =
            system.initStore
                |> system.create myRecord
                |> unwrapJust
    in
    describe "getBy"
        [ test "it returns the right record" <|
            \_ ->
                system.getBy .name myRecord.name store
                    |> Expect.equal (Just myRecord)
        , test "it returns the right id" <|
            \_ ->
                system.getIdBy .name myRecord.name store
                    |> Expect.equal (Just id)
        ]


removeTest : Test
removeTest =
    let
        ( id1, ( id2, store ) ) =
            system.initStore
                |> system.create myRecord
                |> unwrapJust
                |> Tuple.mapSecond
                    (system.create myRecord2
                        >> unwrapJust
                    )
    in
    describe "remove"
        [ test "it removes the record" <|
            \_ ->
                system.remove id1 store
                    |> system.toList
                    |> Expect.equal [ ( id2, myRecord2 ) ]
        , test "the same record can be created again" <|
            \_ ->
                -- tests that indices are updated
                system.remove id1 store
                    |> system.create myRecord
                    |> Expect.notEqual Nothing
        ]
