module ApiDiffTest exposing (suite)

import ApiDiff exposing (ApiDiff, DiffStatus(..), ModuleChanges)
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "ApiDiff"
        [ decoderTests
        , lookupItemTests
        , hasChangesTests
        ]


decoderTests : Test
decoderTests =
    describe "decoder"
        [ test "decodes valid diff JSON with all sections" <|
            \_ ->
                let
                    json =
                        Encode.object
                            [ ( "magnitude", Encode.string "MAJOR" )
                            , ( "addedModules", Encode.list Encode.string [ "NewModule" ] )
                            , ( "removedModules", Encode.list Encode.string [ "OldModule" ] )
                            , ( "changedModules"
                              , Encode.list identity
                                    [ Encode.object
                                        [ ( "name", Encode.string "SomeModule" )
                                        , ( "added", Encode.list Encode.string [ "newFunc" ] )
                                        , ( "changed", Encode.list Encode.string [ "oldFunc" ] )
                                        , ( "removed", Encode.list Encode.string [ "removedFunc" ] )
                                        ]
                                    ]
                              )
                            ]
                in
                case Decode.decodeValue ApiDiff.decoder json of
                    Ok (Just diff) ->
                        Expect.all
                            [ \d -> Expect.equal "MAJOR" d.magnitude
                            , \d -> Expect.equal [ "NewModule" ] d.addedModules
                            , \d -> Expect.equal [ "OldModule" ] d.removedModules
                            , \d -> Expect.equal 1 (List.length d.changedModules)
                            ]
                            diff

                    Ok Nothing ->
                        Expect.fail "Expected Just but got Nothing"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes null diff as Nothing" <|
            \_ ->
                case Decode.decodeValue ApiDiff.decoder Encode.null of
                    Ok Nothing ->
                        Expect.pass

                    Ok (Just _) ->
                        Expect.fail "Expected Nothing but got Just"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes empty diff (no changes)" <|
            \_ ->
                let
                    json =
                        Encode.object
                            [ ( "magnitude", Encode.string "PATCH" )
                            , ( "addedModules", Encode.list Encode.string [] )
                            , ( "removedModules", Encode.list Encode.string [] )
                            , ( "changedModules", Encode.list identity [] )
                            ]
                in
                case Decode.decodeValue ApiDiff.decoder json of
                    Ok (Just diff) ->
                        Expect.all
                            [ \d -> Expect.equal [] d.addedModules
                            , \d -> Expect.equal [] d.removedModules
                            , \d -> Expect.equal [] d.changedModules
                            ]
                            diff

                    Ok Nothing ->
                        Expect.fail "Expected Just but got Nothing"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes module changes with all fields" <|
            \_ ->
                let
                    json =
                        Encode.object
                            [ ( "magnitude", Encode.string "MINOR" )
                            , ( "addedModules", Encode.list Encode.string [] )
                            , ( "removedModules", Encode.list Encode.string [] )
                            , ( "changedModules"
                              , Encode.list identity
                                    [ Encode.object
                                        [ ( "name", Encode.string "MyModule" )
                                        , ( "added", Encode.list Encode.string [ "foo", "bar" ] )
                                        , ( "changed", Encode.list Encode.string [ "baz" ] )
                                        , ( "removed", Encode.list Encode.string [] )
                                        ]
                                    ]
                              )
                            ]
                in
                case Decode.decodeValue ApiDiff.decoder json of
                    Ok (Just diff) ->
                        case diff.changedModules of
                            [ mc ] ->
                                Expect.all
                                    [ \m -> Expect.equal "MyModule" m.name
                                    , \m -> Expect.equal [ "foo", "bar" ] m.added
                                    , \m -> Expect.equal [ "baz" ] m.changed
                                    , \m -> Expect.equal [] m.removed
                                    ]
                                    mc

                            _ ->
                                Expect.fail "Expected exactly one changed module"

                    Ok Nothing ->
                        Expect.fail "Expected Just but got Nothing"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]


sampleDiff : ApiDiff
sampleDiff =
    { magnitude = "MAJOR"
    , addedModules = [ "NewModule" ]
    , removedModules = [ "OldModule" ]
    , changedModules =
        [ { name = "SomeModule"
          , added = [ "newFunc" ]
          , changed = [ "oldFunc" ]
          , removed = [ "removedFunc" ]
          }
        ]
    }


lookupItemTests : Test
lookupItemTests =
    describe "lookupItem"
        [ test "returns Added for added items in a changed module" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "SomeModule" "newFunc"
                    |> Expect.equal Added
        , test "returns Changed for changed items in a changed module" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "SomeModule" "oldFunc"
                    |> Expect.equal Changed
        , test "returns ModuleAdded for any item in an added module" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "NewModule" "anyFunc"
                    |> Expect.equal ModuleAdded
        , test "returns Unchanged for items not in any diff" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "SomeModule" "untouchedFunc"
                    |> Expect.equal Unchanged
        , test "returns Unchanged for items in modules not mentioned in diff" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "UnrelatedModule" "someFunc"
                    |> Expect.equal Unchanged
        , test "returns Unchanged for removed items (they won't appear in docs)" <|
            \_ ->
                ApiDiff.lookupItem sampleDiff "SomeModule" "removedFunc"
                    |> Expect.equal Unchanged
        ]


hasChangesTests : Test
hasChangesTests =
    describe "hasChanges"
        [ test "returns True when there are added modules" <|
            \_ ->
                ApiDiff.hasChanges { magnitude = "MINOR", addedModules = [ "New" ], removedModules = [], changedModules = [] }
                    |> Expect.equal True
        , test "returns True when there are removed modules" <|
            \_ ->
                ApiDiff.hasChanges { magnitude = "MAJOR", addedModules = [], removedModules = [ "Old" ], changedModules = [] }
                    |> Expect.equal True
        , test "returns True when there are changed modules" <|
            \_ ->
                ApiDiff.hasChanges
                    { magnitude = "MAJOR"
                    , addedModules = []
                    , removedModules = []
                    , changedModules = [ { name = "M", added = [ "f" ], changed = [], removed = [] } ]
                    }
                    |> Expect.equal True
        , test "returns False when no changes" <|
            \_ ->
                ApiDiff.hasChanges { magnitude = "PATCH", addedModules = [], removedModules = [], changedModules = [] }
                    |> Expect.equal False
        ]
