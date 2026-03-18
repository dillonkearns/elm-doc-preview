module FindPackageTest exposing (suite)

import Expect
import FindPackage
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


suite : Test
suite =
    describe "findPackageElmJson"
        [ test "finds package elm.json in current directory" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-app"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-app/elm.json" packageElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.projectType "package"
                        )
        , test "skips application elm.json and finds package in parent" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-pkg/examples/demo"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-pkg/examples/demo/elm.json" applicationElmJsonContents
                            |> BackendTaskTest.withFile "/projects/my-pkg/elm.json" packageElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.projectType "package"
                        )
        , test "skips application elm.json two levels up" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-pkg/examples/nested/deep"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-pkg/examples/nested/deep/elm.json" applicationElmJsonContents
                            |> BackendTaskTest.withFile "/projects/my-pkg/examples/nested/elm.json" applicationElmJsonContents
                            |> BackendTaskTest.withFile "/projects/my-pkg/elm.json" packageElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.projectType "package"
                        )
        , test "returns application elm.json if no package found" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-app"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-app/elm.json" applicationElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.projectType "application"
                        )
        , test "returns directory path for found package" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-pkg/examples/demo"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-pkg/examples/demo/elm.json" applicationElmJsonContents
                            |> BackendTaskTest.withFile "/projects/my-pkg/elm.json" packageElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.dir "/projects/my-pkg"
                        )
        , test "returns current dir when package is in cwd" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/my-pkg"
                    |> BackendTaskTest.fromBackendTaskWith
                        (BackendTaskTest.init
                            |> BackendTaskTest.withFile "/projects/my-pkg/elm.json" packageElmJsonContents
                        )
                    |> BackendTaskTest.expectSuccessWith
                        (\result ->
                            Expect.equal result.dir "/projects/my-pkg"
                        )
        , test "fails when no elm.json found anywhere" <|
            \() ->
                FindPackage.findPackageElmJson "/projects/empty"
                    |> BackendTaskTest.fromBackendTaskWith
                        BackendTaskTest.init
                    |> BackendTaskTest.expectFailure
        ]




-- FIXTURES


packageElmJsonContents : String
packageElmJsonContents =
    """{"type":"package","name":"dillonkearns/elm-pages","summary":"A statically typed site generator.","license":"BSD-3-Clause","version":"12.0.0","exposed-modules":["Pages"],"elm-version":"0.19.0 <= v < 0.20.0","dependencies":{"elm/core":"1.0.0 <= v < 2.0.0"},"test-dependencies":{}}"""


applicationElmJsonContents : String
applicationElmJsonContents =
    """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}"""
