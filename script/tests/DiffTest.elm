module DiffTest exposing (suite)

import Dict
import Docs.Diff as Diff exposing (ApiDiff, DiffStatus(..), ModuleStatus(..))
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Docs.Diff"
        [ decoderTests
        , lookupItemTests
        , moduleStatusTests
        ]


sampleDiff : ApiDiff
sampleDiff =
    { magnitude = "MINOR"
    , addedModules = [ "NewModule" ]
    , removedModules = [ "OldModule" ]
    , changedModules =
        [ { name = "ChangedModule"
          , added = [ "newHelper" ]
          , changed = [ "map" ]
          , removed = [ "oldFn" ]
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


decoderTests : Test
decoderTests =
    describe "decoder"
        [ test "decodes a valid ApiDiff JSON" <|
            \() ->
                let
                    json =
                        """
                        {
                            "magnitude": "MINOR",
                            "addedModules": ["NewModule"],
                            "removedModules": ["OldModule"],
                            "changedModules": [
                                {
                                    "name": "ChangedModule",
                                    "added": ["newHelper"],
                                    "changed": ["map"],
                                    "removed": ["oldFn"]
                                }
                            ]
                        }
                        """
                in
                case Decode.decodeString Diff.decoder json of
                    Ok (Just diff) ->
                        Expect.all
                            [ \d -> d.magnitude |> Expect.equal "MINOR"
                            , \d -> d.addedModules |> Expect.equal [ "NewModule" ]
                            , \d -> d.removedModules |> Expect.equal [ "OldModule" ]
                            , \d ->
                                d.changedModules
                                    |> List.map .name
                                    |> Expect.equal [ "ChangedModule" ]
                            ]
                            diff

                    Ok Nothing ->
                        Expect.fail "Expected Just, got Nothing"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes null as Nothing" <|
            \() ->
                case Decode.decodeString Diff.decoder "null" of
                    Ok Nothing ->
                        Expect.pass

                    Ok (Just _) ->
                        Expect.fail "Expected Nothing, got Just"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]


lookupItemTests : Test
lookupItemTests =
    describe "lookupItem"
        [ test "returns Added for item in changedModule's added list" <|
            \() ->
                Diff.lookupItem sampleDiff "ChangedModule" "newHelper"
                    |> Expect.equal Added
        , test "returns Changed for item in changedModule's changed list" <|
            \() ->
                Diff.lookupItem sampleDiff "ChangedModule" "map"
                    |> Expect.equal Changed
        , test "returns Unchanged for item not in any change list" <|
            \() ->
                Diff.lookupItem sampleDiff "ChangedModule" "existingFn"
                    |> Expect.equal Unchanged
        , test "returns ModuleAdded for item in an added module" <|
            \() ->
                Diff.lookupItem sampleDiff "NewModule" "anything"
                    |> Expect.equal ModuleAdded
        , test "returns Unchanged for item in unknown module" <|
            \() ->
                Diff.lookupItem sampleDiff "UnknownModule" "anything"
                    |> Expect.equal Unchanged
        ]


moduleStatusTests : Test
moduleStatusTests =
    describe "moduleStatus"
        [ test "returns ModuleNew for added module" <|
            \() ->
                Diff.moduleStatus sampleDiff "NewModule"
                    |> Expect.equal ModuleNew
        , test "returns ModuleRemoved for removed module" <|
            \() ->
                Diff.moduleStatus sampleDiff "OldModule"
                    |> Expect.equal ModuleRemoved
        , test "returns ModuleChanged for changed module" <|
            \() ->
                Diff.moduleStatus sampleDiff "ChangedModule"
                    |> Expect.equal ModuleChanged
        , test "returns ModuleUnchanged for module with no changes" <|
            \() ->
                Diff.moduleStatus sampleDiff "SomeOtherModule"
                    |> Expect.equal ModuleUnchanged
        ]
