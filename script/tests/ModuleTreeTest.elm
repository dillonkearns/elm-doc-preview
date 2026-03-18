module ModuleTreeTest exposing (suite)

import Expect
import ModuleTree exposing (TreeEntry(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ModuleTree"
        [ describe "build"
            [ test "groups modules with shared prefix" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Json.Encode", "Http" ]
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Group { prefix = "Json", expanded = True, depth = 0 }
                            , Leaf { name = "Json.Decode", depth = 1 }
                            , Leaf { name = "Json.Encode", depth = 1 }
                            , Leaf { name = "Http", depth = 0 }
                            ]
            , test "single-module namespace has no group" <|
                \() ->
                    ModuleTree.build [ "Http", "Platform" ]
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Leaf { name = "Http", depth = 0 }
                            , Leaf { name = "Platform", depth = 0 }
                            ]
            , test "sorts groups alphabetically" <|
                \() ->
                    ModuleTree.build [ "Html.Attributes", "Html.Events", "Json.Decode", "Json.Encode" ]
                        |> ModuleTree.visibleEntries
                        |> List.filterMap
                            (\entry ->
                                case entry of
                                    Group { prefix } ->
                                        Just prefix

                                    Leaf _ ->
                                        Nothing
                            )
                        |> Expect.equal [ "Html", "Json" ]
            , test "empty list produces empty tree" <|
                \() ->
                    ModuleTree.build []
                        |> ModuleTree.visibleEntries
                        |> Expect.equal []
            , test "single module has no group wrapper" <|
                \() ->
                    ModuleTree.build [ "Json.Decode" ]
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Leaf { name = "Json.Decode", depth = 0 }
                            ]
            ]
        , describe "toggle"
            [ test "collapsing group hides children" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Json.Encode" ]
                        |> ModuleTree.toggle "Json"
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Group { prefix = "Json", expanded = False, depth = 0 }
                            ]
            , test "expanding collapsed group shows children" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Json.Encode" ]
                        |> ModuleTree.toggle "Json"
                        |> ModuleTree.toggle "Json"
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Group { prefix = "Json", expanded = True, depth = 0 }
                            , Leaf { name = "Json.Decode", depth = 1 }
                            , Leaf { name = "Json.Encode", depth = 1 }
                            ]
            , test "toggling non-existent group is no-op" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Json.Encode" ]
                        |> ModuleTree.toggle "Nonexistent"
                        |> ModuleTree.visibleEntries
                        |> Expect.equal
                            [ Group { prefix = "Json", expanded = True, depth = 0 }
                            , Leaf { name = "Json.Decode", depth = 1 }
                            , Leaf { name = "Json.Encode", depth = 1 }
                            ]
            ]
        , describe "filter"
            [ test "filters entries by case-insensitive substring" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Json.Encode", "Http" ]
                        |> ModuleTree.filter "json"
                        |> ModuleTree.visibleEntries
                        |> List.filterMap
                            (\entry ->
                                case entry of
                                    Leaf { name } ->
                                        Just name

                                    Group _ ->
                                        Nothing
                            )
                        |> Expect.equal [ "Json.Decode", "Json.Encode" ]
            , test "empty filter shows all" <|
                \() ->
                    ModuleTree.build [ "Json.Decode", "Http" ]
                        |> ModuleTree.filter ""
                        |> ModuleTree.visibleEntries
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "entryName"
            [ test "leaf name" <|
                \() ->
                    ModuleTree.entryName (Leaf { name = "Json.Decode", depth = 0 })
                        |> Expect.equal "Json.Decode"
            , test "group name" <|
                \() ->
                    ModuleTree.entryName (Group { prefix = "Json", expanded = True, depth = 0 })
                        |> Expect.equal "Json"
            ]
        ]
