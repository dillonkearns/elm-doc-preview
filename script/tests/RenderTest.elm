module RenderTest exposing (suite)

import Dict
import Docs.Diff as Diff exposing (DiffStatus(..))
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Docs.Render"
        [ typeSignatureTests
        , valueRenderTests
        , unionRenderTests
        , aliasRenderTests
        , binopRenderTests
        , tocTests
        , moduleRenderTests
        , multiLineTypeTests
        , diffRenderTests
        ]


typeSignatureTests : Test
typeSignatureTests =
    describe "typeToString"
        [ test "simple type Int" <|
            \() ->
                Render.typeToString (Type.Type "Basics.Int" [])
                    |> Expect.equal "Int"
        , test "function type a -> b" <|
            \() ->
                Render.typeToString (Type.Lambda (Type.Var "a") (Type.Var "b"))
                    |> Expect.equal "a -> b"
        , test "multi-arg function String -> Int -> Bool" <|
            \() ->
                Render.typeToString
                    (Type.Lambda
                        (Type.Type "String.String" [])
                        (Type.Lambda (Type.Type "Basics.Int" []) (Type.Type "Basics.Bool" []))
                    )
                    |> Expect.equal "String -> Int -> Bool"
        , test "parameterized type Maybe a" <|
            \() ->
                Render.typeToString (Type.Type "Maybe.Maybe" [ Type.Var "a" ])
                    |> Expect.equal "Maybe a"
        , test "nested type List (Maybe a)" <|
            \() ->
                Render.typeToString
                    (Type.Type "List.List" [ Type.Type "Maybe.Maybe" [ Type.Var "a" ] ])
                    |> Expect.equal "List (Maybe a)"
        , test "record type { x : Float, y : Float }" <|
            \() ->
                Render.typeToString
                    (Type.Record
                        [ ( "x", Type.Type "Basics.Float" [] )
                        , ( "y", Type.Type "Basics.Float" [] )
                        ]
                        Nothing
                    )
                    |> Expect.equal "{ x : Float, y : Float }"
        , test "extensible record { a | x : Int }" <|
            \() ->
                Render.typeToString
                    (Type.Record
                        [ ( "x", Type.Type "Basics.Int" [] ) ]
                        (Just "a")
                    )
                    |> Expect.equal "{ a | x : Int }"
        , test "tuple ( a, b )" <|
            \() ->
                Render.typeToString
                    (Type.Tuple [ Type.Var "a", Type.Var "b" ])
                    |> Expect.equal "( a, b )"
        , test "complex nested: Result (List String) (Maybe Int)" <|
            \() ->
                Render.typeToString
                    (Type.Type "Result.Result"
                        [ Type.Type "List.List" [ Type.Type "String.String" [] ]
                        , Type.Type "Maybe.Maybe" [ Type.Type "Basics.Int" [] ]
                        ]
                    )
                    |> Expect.equal "Result (List String) (Maybe Int)"
        , test "unit type ()" <|
            \() ->
                Render.typeToString (Type.Tuple [])
                    |> Expect.equal "()"
        , test "type variable" <|
            \() ->
                Render.typeToString (Type.Var "comparable")
                    |> Expect.equal "comparable"
        , test "lambda in app context gets parens" <|
            \() ->
                Render.typeToString
                    (Type.Type "List.List"
                        [ Type.Lambda (Type.Var "a") (Type.Var "b") ]
                    )
                    |> Expect.equal "List (a -> b)"
        ]


valueRenderTests : Test
valueRenderTests =
    describe "renderValue"
        [ test "renders value with name, type signature, and doc comment" <|
            \() ->
                let
                    value =
                        { name = "map"
                        , comment = "Apply a function to every element of a list."
                        , tipe =
                            Type.Lambda
                                (Type.Lambda (Type.Var "a") (Type.Var "b"))
                                (Type.Lambda
                                    (Type.Type "List.List" [ Type.Var "a" ])
                                    (Type.Type "List.List" [ Type.Var "b" ])
                                )
                        }

                    result =
                        Render.renderValue value
                in
                result
                    |> String.contains "map"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains ":"
                            |> Expect.equal True
                        )
        , test "rendered value contains the type signature" <|
            \() ->
                let
                    value =
                        { name = "identity"
                        , comment = "Return the argument."
                        , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                        }
                in
                Render.renderValue value
                    |> String.contains "a -> a"
                    |> Expect.equal True
        ]


unionRenderTests : Test
unionRenderTests =
    describe "renderUnion"
        [ test "renders union type with constructors" <|
            \() ->
                let
                    union =
                        { name = "Maybe"
                        , comment = "Represent values that may or may not exist."
                        , args = [ "a" ]
                        , tags =
                            [ ( "Just", [ Type.Var "a" ] )
                            , ( "Nothing", [] )
                            ]
                        }

                    result =
                        Render.renderUnion union
                in
                result
                    |> String.contains "type Maybe a"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "Just a"
                            |> Expect.equal True
                        )
                    |> always
                        (result
                            |> String.contains "Nothing"
                            |> Expect.equal True
                        )
        , test "renders union with = and | separators" <|
            \() ->
                let
                    union =
                        { name = "Bool"
                        , comment = "A boolean value."
                        , args = []
                        , tags =
                            [ ( "True", [] )
                            , ( "False", [] )
                            ]
                        }

                    result =
                        Render.renderUnion union
                in
                result
                    |> String.contains "="
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "|"
                            |> Expect.equal True
                        )
        ]


aliasRenderTests : Test
aliasRenderTests =
    describe "renderAlias"
        [ test "renders type alias with body" <|
            \() ->
                let
                    alias =
                        { name = "Point"
                        , comment = "A two-dimensional point."
                        , args = []
                        , tipe =
                            Type.Record
                                [ ( "x", Type.Type "Basics.Float" [] )
                                , ( "y", Type.Type "Basics.Float" [] )
                                ]
                                Nothing
                        }

                    result =
                        Render.renderAlias alias
                in
                result
                    |> String.contains "type alias Point"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "="
                            |> Expect.equal True
                        )
        ]


binopRenderTests : Test
binopRenderTests =
    describe "renderBinop"
        [ test "renders binary operator with type and precedence" <|
            \() ->
                let
                    binop =
                        { name = "+"
                        , comment = "Add numbers."
                        , tipe =
                            Type.Lambda (Type.Var "number")
                                (Type.Lambda (Type.Var "number") (Type.Var "number"))
                        , associativity = Docs.Left
                        , precedence = 6
                        }

                    result =
                        Render.renderBinop binop
                in
                result
                    |> String.contains "(+)"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "number -> number -> number"
                            |> Expect.equal True
                        )
        ]


tocTests : Test
tocTests =
    describe "renderToc"
        [ test "renders table of contents with module names and summaries" <|
            \() ->
                let
                    modules =
                        [ { name = "List"
                          , comment = "Manipulate lists of values.\n\n@docs map"
                          , unions = []
                          , aliases = []
                          , values = []
                          , binops = []
                          }
                        , { name = "Dict"
                          , comment = "A dictionary mapping unique keys to values.\n\n@docs empty"
                          , unions = []
                          , aliases = []
                          , values = []
                          , binops = []
                          }
                        ]

                    result =
                        Render.renderToc modules
                in
                result
                    |> String.contains "List"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "Manipulate lists of values."
                            |> Expect.equal True
                        )
                    |> always
                        (result
                            |> String.contains "Dict"
                            |> Expect.equal True
                        )
        , test "module with empty comment shows module name with no summary" <|
            \() ->
                let
                    modules =
                        [ { name = "Internal"
                          , comment = ""
                          , unions = []
                          , aliases = []
                          , values = []
                          , binops = []
                          }
                        ]

                    result =
                        Render.renderToc modules
                in
                result
                    |> String.contains "Internal"
                    |> Expect.equal True
        , test "first sentence extraction stops at period followed by space or newline" <|
            \() ->
                let
                    modules =
                        [ { name = "String"
                          , comment = "A built-in representation for efficient string manipulation. Second sentence here."
                          , unions = []
                          , aliases = []
                          , values = []
                          , binops = []
                          }
                        ]

                    result =
                        Render.renderToc modules
                in
                result
                    |> String.contains "A built-in representation for efficient string manipulation."
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "Second sentence here."
                            |> Expect.equal False
                        )
        ]


multiLineTypeTests : Test
multiLineTypeTests =
    describe "multi-line type formatting"
        [ test "short type stays on one line in renderValue" <|
            \() ->
                Render.renderValue
                    { name = "id"
                    , comment = "doc"
                    , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                    }
                    |> String.contains "id"
                    |> Expect.equal True
        , test "long function type wraps across lines with arrows" <|
            \() ->
                let
                    result =
                        Render.renderValue
                            { name = "makeInfo"
                            , comment = "doc"
                            , tipe =
                                Type.Lambda (Type.Type "String.String" [])
                                    (Type.Lambda (Type.Type "String.String" [])
                                        (Type.Lambda (Type.Type "Maybe.Maybe" [ Type.Type "Elm.Version.Version" [] ])
                                            (Type.Lambda (Type.Type "Maybe.Maybe" [ Type.Type "String.String" [] ])
                                                (Type.Lambda (Type.Type "String.String" [])
                                                    (Type.Type "Basics.Bool" [])
                                                )
                                            )
                                        )
                                    )
                            }
                in
                result
                    |> String.contains "-> String"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "-> Maybe Version"
                            |> Expect.equal True
                        )
        , test "long record type breaks per field" <|
            \() ->
                let
                    result =
                        Render.renderAlias
                            { name = "Config"
                            , comment = "doc"
                            , args = []
                            , tipe =
                                Type.Record
                                    [ ( "author", Type.Type "String.String" [] )
                                    , ( "project", Type.Type "String.String" [] )
                                    , ( "version", Type.Type "Maybe.Maybe" [ Type.Type "Elm.Version.Version" [] ] )
                                    , ( "moduleName", Type.Type "String.String" [] )
                                    ]
                                    Nothing
                            }
                in
                result
                    |> String.contains ", project : String"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "{ author : String"
                            |> Expect.equal True
                        )
        , test "record arg in function type breaks per field" <|
            \() ->
                let
                    result =
                        Render.renderValue
                            { name = "generate"
                            , comment = "doc"
                            , tipe =
                                Type.Lambda (Type.Type "Config.Config" [])
                                    (Type.Lambda
                                        (Type.Record
                                            [ ( "events", Type.Type "List.List" [ Type.Type "Event.Event" [] ] )
                                            , ( "journals", Type.Type "List.List" [ Type.Type "Journal.Journal" [] ] )
                                            ]
                                            Nothing
                                        )
                                        (Type.Type "String.String" [])
                                    )
                            }
                in
                result
                    |> String.contains "events"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "journals"
                            |> Expect.equal True
                        )
        ]


moduleRenderTests : Test
moduleRenderTests =
    describe "renderModule"
        [ test "renders module with header and blocks" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Module intro.\n\n@docs identity"
                        , unions = []
                        , aliases = []
                        , values =
                            [ { name = "identity"
                              , comment = "Return the argument unchanged."
                              , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                              }
                            ]
                        , binops = []
                        }

                    result =
                        Render.renderModule module_
                in
                result
                    |> String.contains "MyModule"
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "identity"
                            |> Expect.equal True
                        )
                    |> always
                        (result
                            |> String.contains "a -> a"
                            |> Expect.equal True
                        )
        , test "markdown blocks appear in output" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "This is the intro text.\n\n@docs identity\n\nSome more text."
                        , unions = []
                        , aliases = []
                        , values =
                            [ { name = "identity"
                              , comment = "Docs here."
                              , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                              }
                            ]
                        , binops = []
                        }

                    result =
                        Render.renderModule module_
                in
                result
                    |> String.contains "This is the intro text."
                    |> Expect.equal True
                    |> always
                        (result
                            |> String.contains "Some more text."
                            |> Expect.equal True
                        )
        ]


sampleDiff : Diff.ApiDiff
sampleDiff =
    { magnitude = "MINOR"
    , addedModules = [ "NewModule" ]
    , removedModules = [ "RemovedModule" ]
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


diffRenderTests : Test
diffRenderTests =
    describe "diff-aware rendering"
        [ describe "magnitudeBanner"
            [ test "MAJOR banner contains MAJOR CHANGE in red" <|
                \() ->
                    let
                        result =
                            Render.magnitudeBanner "MAJOR"
                    in
                    Expect.all
                        [ \s -> s |> String.contains "MAJOR CHANGE" |> Expect.equal True
                        , \s -> s |> String.contains "[31m" |> Expect.equal True
                        ]
                        result
            , test "MINOR banner contains MINOR CHANGE in green" <|
                \() ->
                    let
                        result =
                            Render.magnitudeBanner "MINOR"
                    in
                    Expect.all
                        [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                        , \s -> s |> String.contains "[32m" |> Expect.equal True
                        ]
                        result
            , test "PATCH banner contains PATCH CHANGE in cyan" <|
                \() ->
                    let
                        result =
                            Render.magnitudeBanner "PATCH"
                    in
                    Expect.all
                        [ \s -> s |> String.contains "PATCH CHANGE" |> Expect.equal True
                        , \s -> s |> String.contains "[36m" |> Expect.equal True
                        ]
                        result
            ]
        , describe "renderBlockWithDiff"
            [ test "Added value has green background + badge" <|
                \() ->
                    let
                        block =
                            Docs.ValueBlock
                                { name = "newFn"
                                , comment = "A new function."
                                , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                                }

                        result =
                            Render.renderBlockWithDiff sampleDiff "SomeModule" Added block
                    in
                    Expect.all
                        [ \s -> s |> String.contains "+" |> Expect.equal True
                        , \s -> s |> String.contains "[42m" |> Expect.equal True
                        ]
                        result
            , test "Changed value has yellow background ~ badge" <|
                \() ->
                    let
                        block =
                            Docs.ValueBlock
                                { name = "map"
                                , comment = "Map over things."
                                , tipe = Type.Lambda (Type.Var "a") (Type.Var "b")
                                }

                        result =
                            Render.renderBlockWithDiff sampleDiff "SomeModule" Changed block
                    in
                    Expect.all
                        [ \s -> s |> String.contains "~" |> Expect.equal True
                        , \s -> s |> String.contains "[43m" |> Expect.equal True
                        ]
                        result
            , test "Unchanged value has no prefix marker" <|
                \() ->
                    let
                        block =
                            Docs.ValueBlock
                                { name = "identity"
                                , comment = "Return the argument."
                                , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                                }

                        result =
                            Render.renderBlockWithDiff sampleDiff "SomeModule" Unchanged block
                    in
                    result
                        |> String.startsWith "  "
                        |> Expect.equal True
            ]
        , describe "renderRemovedItem"
            [ test "renders removed item with red background badge and strikethrough" <|
                \() ->
                    let
                        result =
                            Render.renderRemovedItem "oldFn"
                    in
                    Expect.all
                        [ \s -> s |> String.contains "-" |> Expect.equal True
                        , \s -> s |> String.contains "oldFn" |> Expect.equal True
                        , \s -> s |> String.contains "[41m" |> Expect.equal True
                        , \s -> s |> String.contains "[9m" |> Expect.equal True
                        ]
                        result
            ]
        , describe "renderTocWithDiff"
            [ test "TOC only shows changed modules, not unchanged ones" <|
                \() ->
                    let
                        modules =
                            [ { name = "NewModule"
                              , comment = "A newly added module."
                              , unions = []
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            , { name = "ChangedModule"
                              , comment = "This module has changes."
                              , unions = []
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            , { name = "UnchangedModule"
                              , comment = "No changes here."
                              , unions = []
                              , aliases = []
                              , values = []
                              , binops = []
                              }
                            ]

                        result =
                            Render.renderTocWithDiff sampleDiff modules
                    in
                    Expect.all
                        [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                        , \s -> s |> String.contains "NewModule" |> Expect.equal True
                        , \s -> s |> String.contains "ChangedModule" |> Expect.equal True
                        , \s -> s |> String.contains "RemovedModule" |> Expect.equal True
                        , \s -> s |> String.contains "UnchangedModule" |> Expect.equal False
                        ]
                        result
            ]
        , describe "renderModuleWithDiff"
            [ test "module view only shows changed items, not unchanged ones" <|
                \() ->
                    let
                        module_ =
                            { name = "ChangedModule"
                            , comment = "A module.\n\n@docs map, newHelper, stable"
                            , unions = []
                            , aliases = []
                            , values =
                                [ { name = "map"
                                  , comment = "Map function."
                                  , tipe = Type.Lambda (Type.Var "a") (Type.Var "b")
                                  }
                                , { name = "newHelper"
                                  , comment = "A new helper."
                                  , tipe = Type.Type "Basics.Int" []
                                  }
                                , { name = "stable"
                                  , comment = "Unchanged function."
                                  , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                                  }
                                ]
                            , binops = []
                            }

                        result =
                            Render.renderModuleWithDiff sampleDiff module_
                    in
                    Expect.all
                        [ \s -> s |> String.contains "ChangedModule" |> Expect.equal True
                        , \s -> s |> String.contains "map" |> Expect.equal True
                        , \s -> s |> String.contains "newHelper" |> Expect.equal True
                        , \s -> s |> String.contains "oldFn" |> Expect.equal True
                        , \s -> s |> String.contains "stable" |> Expect.equal False
                        ]
                        result
            ]
        , describe "renderFullDiff"
            [ test "full diff shows banner and all changed modules expanded, not unchanged" <|
                \() ->
                    let
                        modules =
                            [ { name = "ChangedModule"
                              , comment = "A module.\n\n@docs map, newHelper"
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "map"
                                      , comment = "Map function."
                                      , tipe = Type.Lambda (Type.Var "a") (Type.Var "b")
                                      }
                                    , { name = "newHelper"
                                      , comment = "A new helper."
                                      , tipe = Type.Type "Basics.Int" []
                                      }
                                    ]
                              , binops = []
                              }
                            , { name = "UnchangedModule"
                              , comment = "No changes.\n\n@docs stable"
                              , unions = []
                              , aliases = []
                              , values =
                                    [ { name = "stable"
                                      , comment = "Stable."
                                      , tipe = Type.Var "a"
                                      }
                                    ]
                              , binops = []
                              }
                            ]

                        result =
                            Render.renderFullDiff sampleDiff modules
                    in
                    Expect.all
                        [ \s -> s |> String.contains "MINOR CHANGE" |> Expect.equal True
                        , \s -> s |> String.contains "ChangedModule" |> Expect.equal True
                        , \s -> s |> String.contains "map" |> Expect.equal True
                        , \s -> s |> String.contains "newHelper" |> Expect.equal True
                        , \s -> s |> String.contains "oldFn" |> Expect.equal True
                        , \s -> s |> String.contains "RemovedModule" |> Expect.equal True
                        , \s -> s |> String.contains "UnchangedModule" |> Expect.equal False
                        , \s -> s |> String.contains "stable" |> Expect.equal False
                        ]
                        result
            ]
        ]
