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
                                    collectStdout outputs
                            in
                            Expect.all
                                [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "OtherModule" |> Expect.equal True
                                ]
                                allOutput
                        )
                    |> Test.BackendTask.expectSuccess
        , test "Module mode: --module shows full module docs" <|
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
                            Expect.all
                                [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                , \s -> s |> String.contains "identity" |> Expect.equal True
                                , \s -> s |> String.contains "a -> a" |> Expect.equal True
                                ]
                                (collectStdout outputs)
                        )
                    |> Test.BackendTask.expectSuccess
        , test "Module mode: non-existent module fails" <|
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
                            body |> String.contains "NonExistent" |> Expect.equal True
                        )
        , describe "diff refs mode"
            [ test "full worktree lifecycle for package diff" <|
                \() ->
                    Test.BackendTask.fromScriptWith
                        (Test.BackendTask.init
                            |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/docs.json" fixtureBaseDocsJson
                        )
                        [ "--diff", "v1.0.0..v2.0.0" ]
                        ElmDocs.run
                        |> Test.BackendTask.simulateCommand "elm" ""
                        |> Test.BackendTask.simulateCommand "mktemp" "/tmp/test-worktree"
                        |> Test.BackendTask.simulateCommand "rm" ""
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.simulateCommand "bash" ""
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.simulateCustom "computeDiff" fixtureDiffValue
                        |> Test.BackendTask.ensureOutputWith
                            (\outputs ->
                                Expect.all
                                    [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                                    , \s -> s |> String.contains "MyModule" |> Expect.equal True
                                    ]
                                    (collectStdout outputs)
                            )
                        |> Test.BackendTask.expectSuccess
            , test "git worktree add receives correct ref" <|
                \() ->
                    Test.BackendTask.fromScriptWith
                        (Test.BackendTask.init
                            |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/docs.json" fixtureBaseDocsJson
                        )
                        [ "--diff", "v1.0.0..v2.0.0" ]
                        ElmDocs.run
                        |> Test.BackendTask.simulateCommand "elm" ""
                        |> Test.BackendTask.simulateCommand "mktemp" "/tmp/test-worktree"
                        |> Test.BackendTask.simulateCommand "rm" ""
                        |> Test.BackendTask.ensureCommand "git"
                            (\args ->
                                Expect.equal [ "worktree", "add", "--detach", "/tmp/test-worktree", "v1.0.0" ] args
                            )
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.ensureCommand "bash"
                            (\args ->
                                case args of
                                    [ "-c", cmd ] ->
                                        cmd |> String.contains "elm make --docs=docs.json" |> Expect.equal True

                                    _ ->
                                        Expect.fail ("Expected bash -c, got: " ++ String.join " " args)
                            )
                        |> Test.BackendTask.simulateCommand "bash" ""
                        |> Test.BackendTask.ensureCommand "git"
                            (\args ->
                                Expect.equal [ "worktree", "remove", "--force", "/tmp/test-worktree" ] args
                            )
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.simulateCustom "computeDiff" fixtureDiffValue
                        |> Test.BackendTask.expectSuccess
            , test "diff with --module shows annotated module view" <|
                \() ->
                    Test.BackendTask.fromScriptWith
                        (Test.BackendTask.init
                            |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "/tmp/test-worktree/docs.json" fixtureBaseDocsJson
                        )
                        [ "--diff", "v1.0.0..v2.0.0", "--module", "MyModule" ]
                        ElmDocs.run
                        |> Test.BackendTask.simulateCommand "elm" ""
                        |> Test.BackendTask.simulateCommand "mktemp" "/tmp/test-worktree"
                        |> Test.BackendTask.simulateCommand "rm" ""
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.simulateCommand "bash" ""
                        |> Test.BackendTask.simulateCommand "git" ""
                        |> Test.BackendTask.simulateCustom "computeDiff" fixtureDiffValue
                        |> Test.BackendTask.ensureOutputWith
                            (\outputs ->
                                Expect.all
                                    [ \s -> s |> String.contains "MyModule" |> Expect.equal True
                                    , \s -> s |> String.contains "identity" |> Expect.equal True
                                    ]
                                    (collectStdout outputs)
                            )
                        |> Test.BackendTask.expectSuccess
            ]
        , describe "diff published mode"
            [ test "fetches published docs via HTTP and computes diff" <|
                \() ->
                    Test.BackendTask.fromScriptWith
                        (Test.BackendTask.init
                            |> Test.BackendTask.withFile "elm.json" fixturePackageElmJson
                            |> Test.BackendTask.withFile "docs.json" fixtureDocsJson
                        )
                        [ "--diff", "published" ]
                        ElmDocs.run
                        |> Test.BackendTask.simulateCommand "elm" ""
                        |> Test.BackendTask.simulateHttpGet
                            "https://package.elm-lang.org/packages/author/project/1.0.0/docs.json"
                            (Encode.string fixtureBaseDocsJson)
                        |> Test.BackendTask.simulateCustom "computeDiff" fixtureDiffValue
                        |> Test.BackendTask.ensureOutputWith
                            (\outputs ->
                                Expect.all
                                    [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                                    , \s -> s |> String.contains "MyModule" |> Expect.equal True
                                    ]
                                    (collectStdout outputs)
                            )
                        |> Test.BackendTask.expectSuccess
            , test "published diff fails for applications" <|
                \() ->
                    Test.BackendTask.fromScriptWith
                        (Test.BackendTask.init
                            |> Test.BackendTask.withFile "elm.json" fixtureApplicationElmJson
                        )
                        [ "--diff", "published" ]
                        ElmDocs.run
                        |> Test.BackendTask.simulateCustom "buildApplicationDocs" (Encode.string fixtureDocsJson)
                        |> Test.BackendTask.expectFailureWith
                            (\{ body } ->
                                body |> String.contains "packages with a published version" |> Expect.equal True
                            )
            ]
        ]


collectStdout : List Test.BackendTask.Output -> String
collectStdout outputs =
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


fixtureDiffValue : Encode.Value
fixtureDiffValue =
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


fixtureApplicationElmJson : String
fixtureApplicationElmJson =
    Encode.encode 0
        (Encode.object
            [ ( "type", Encode.string "application" )
            , ( "source-directories", Encode.list Encode.string [ "src" ] )
            , ( "elm-version", Encode.string "0.19.1" )
            , ( "dependencies"
              , Encode.object
                    [ ( "direct"
                      , Encode.object [ ( "elm/core", Encode.string "1.0.5" ) ]
                      )
                    , ( "indirect", Encode.object [] )
                    ]
              )
            , ( "test-dependencies"
              , Encode.object
                    [ ( "direct", Encode.object [] )
                    , ( "indirect", Encode.object [] )
                    ]
              )
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


fixtureBaseDocsJson : String
fixtureBaseDocsJson =
    Encode.encode 0
        (Encode.list identity
            [ Encode.object
                [ ( "name", Encode.string "MyModule" )
                , ( "comment", Encode.string "A module for doing things.\n\n@docs oldFn" )
                , ( "unions", Encode.list identity [] )
                , ( "aliases", Encode.list identity [] )
                , ( "values"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "oldFn" )
                            , ( "comment", Encode.string "An old function." )
                            , ( "type", Encode.string "String -> String" )
                            ]
                        ]
                  )
                , ( "binops", Encode.list identity [] )
                ]
            ]
        )
