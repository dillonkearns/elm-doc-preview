module ElmDocsTest exposing (suite)

import ElmDocs
import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask


suite : Test
suite =
    describe "ElmDocs script"
        [ test "TOC mode: no args shows module names from docs.json" <|
            \() ->
                Test.BackendTask.fromScriptWith
                    (Test.BackendTask.init
                        |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                        |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                    )
                    []
                    ElmDocs.run
                    |> Test.BackendTask.simulateCommand "elm" ""
                    |> Test.BackendTask.ensureOutputWith
                        (\outputs ->
                            let
                                allOutput =
                                    outputs
                                        |> List.filterMap
                                            (\output ->
                                                case output of
                                                    Test.BackendTask.Stdout s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )
                                        |> String.join "\n"
                            in
                            Expect.all
                                [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "OtherModule" |> Expect.equal True
                                ]
                                allOutput
                        )
                    |> Test.BackendTask.expectSuccess
        , test "Module mode: --module shows full module docs with type signatures" <|
            \() ->
                Test.BackendTask.fromScriptWith
                    (Test.BackendTask.init
                        |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                        |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                    )
                    [ "--module", "MyModule" ]
                    ElmDocs.run
                    |> Test.BackendTask.simulateCommand "elm" ""
                    |> Test.BackendTask.ensureOutputWith
                        (\outputs ->
                            let
                                allOutput =
                                    outputs
                                        |> List.filterMap
                                            (\output ->
                                                case output of
                                                    Test.BackendTask.Stdout s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )
                                        |> String.join "\n"
                            in
                            Expect.all
                                [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "identity" |> Expect.equal True
                                , \s -> s |> String.contains "a -> a" |> Expect.equal True
                                ]
                                allOutput
                        )
                    |> Test.BackendTask.expectSuccess
        , test "Module mode: non-existent module fails with helpful error" <|
            \() ->
                Test.BackendTask.fromScriptWith
                    (Test.BackendTask.init
                        |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                        |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                    )
                    [ "--module", "NonExistent" ]
                    ElmDocs.run
                    |> Test.BackendTask.simulateCommand "elm" ""
                    |> Test.BackendTask.expectFailureWith
                        (\{ body } ->
                            body
                                |> String.contains "NonExistent"
                                |> Expect.equal True
                        )
        , test "Diff mode: --diff with refs shows diff TOC with magnitude banner" <|
            \() ->
                Test.BackendTask.fromScriptWith
                    (Test.BackendTask.init
                        |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                        |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                    )
                    [ "--diff", "base..head" ]
                    ElmDocs.run
                    |> Test.BackendTask.simulateCommand "elm" ""
                    |> Test.BackendTask.simulateCustom "computeApiDiff" fixtureDiffJson
                    |> Test.BackendTask.ensureOutputWith
                        (\outputs ->
                            let
                                allOutput =
                                    outputs
                                        |> List.filterMap
                                            (\output ->
                                                case output of
                                                    Test.BackendTask.Stdout s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )
                                        |> String.join "\n"
                            in
                            Expect.all
                                [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                                , \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "OtherModule" |> Expect.equal True
                                ]
                                allOutput
                        )
                    |> Test.BackendTask.expectSuccess
        , test "Diff mode: --diff with module shows diff-annotated module view" <|
            \() ->
                Test.BackendTask.fromScriptWith
                    (Test.BackendTask.init
                        |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                        |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                    )
                    [ "--diff", "base..head", "--module", "MyModule" ]
                    ElmDocs.run
                    |> Test.BackendTask.simulateCommand "elm" ""
                    |> Test.BackendTask.simulateCustom "computeApiDiff" fixtureDiffJson
                    |> Test.BackendTask.ensureOutputWith
                        (\outputs ->
                            let
                                allOutput =
                                    outputs
                                        |> List.filterMap
                                            (\output ->
                                                case output of
                                                    Test.BackendTask.Stdout s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )
                                        |> String.join "\n"
                            in
                            Expect.all
                                [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "identity" |> Expect.equal True
                                ]
                                allOutput
                        )
                    |> Test.BackendTask.expectSuccess
        ]


fixtureDiffJson : Encode.Value
fixtureDiffJson =
    Encode.object
        [ ( "magnitude", Encode.string "MINOR" )
        , ( "addedModules", Encode.list Encode.string [ "OtherModule" ] )
        , ( "removedModules", Encode.list Encode.string [] )
        , ( "changedModules"
          , Encode.list identity
                [ Encode.object
                    [ ( "name", Encode.string "MyModule" )
                    , ( "added", Encode.list Encode.string [ "identity" ] )
                    , ( "changed", Encode.list Encode.string [] )
                    , ( "removed", Encode.list Encode.string [] )
                    ]
                ]
          )
        ]


fixturePackageElmJson : String
fixturePackageElmJson =
    Encode.encode 0
        (Encode.object
            [ ( "type", Encode.string "package" )
            , ( "name", Encode.string "author/project" )
            , ( "summary", Encode.string "A test package" )
            , ( "license", Encode.string "BSD-3-Clause" )
            , ( "version", Encode.string "1.0.0" )
            , ( "exposed-modules", Encode.list Encode.string [ "MyModule", "OtherModule" ] )
            , ( "elm-version", Encode.string "0.19.0 <= v < 0.20.0" )
            , ( "dependencies"
              , Encode.object
                    [ ( "elm/core", Encode.string "1.0.0 <= v < 2.0.0" ) ]
              )
            , ( "test-dependencies", Encode.object [] )
            ]
        )


fixtureDocsJson : String
fixtureDocsJson =
    Encode.encode 0
        (Encode.list identity
            [ Encode.object
                [ ( "name", Encode.string "MyModule" )
                , ( "comment", Encode.string "A module for doing things.\n\n@docs identity" )
                , ( "unions", Encode.list identity [] )
                , ( "aliases", Encode.list identity [] )
                , ( "values"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "identity" )
                            , ( "comment", Encode.string "Return the argument unchanged." )
                            , ( "type", Encode.string "a -> a" )
                            ]
                        ]
                  )
                , ( "binops", Encode.list identity [] )
                ]
            , Encode.object
                [ ( "name", Encode.string "OtherModule" )
                , ( "comment", Encode.string "Another module.\n\n@docs hello" )
                , ( "unions", Encode.list identity [] )
                , ( "aliases", Encode.list identity [] )
                , ( "values"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "hello" )
                            , ( "comment", Encode.string "Say hello." )
                            , ( "type", Encode.string "String" )
                            ]
                        ]
                  )
                , ( "binops", Encode.list identity [] )
                ]
            ]
        )
