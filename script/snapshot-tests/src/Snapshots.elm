module Snapshots exposing (run)

import Docs.Diff as Diff
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import Pages.Script as Script exposing (Script)
import Snapshot


run : Script
run =
    Snapshot.run "Snapshots"
        [ tocSnapshots
        , moduleViewSnapshots
        , typeSignatureSnapshots
        , codeExampleSnapshots
        , multiLineTypeSnapshots
        , diffSnapshots
        ]


tocSnapshots : Snapshot.Test
tocSnapshots =
    Snapshot.describe "TOC"
        [ Snapshot.test "three modules" <|
            \() ->
                Render.renderToc
                    [ { name = "List"
                      , comment = "Manipulate lists of values.\n\n@docs map, filter"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    , { name = "Dict"
                      , comment = "A dictionary mapping unique keys to values.\n\n@docs empty, insert"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    , { name = "String"
                      , comment = "A built-in representation for efficient string manipulation. Second sentence here.\n\n@docs isEmpty"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    ]
        , Snapshot.test "empty module comment" <|
            \() ->
                Render.renderToc
                    [ { name = "Internal"
                      , comment = ""
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    ]
        ]


moduleViewSnapshots : Snapshot.Test
moduleViewSnapshots =
    Snapshot.describe "Module view"
        [ Snapshot.test "full module with values unions and aliases" <|
            \() ->
                Render.renderModule
                    { name = "MyModule"
                    , comment = "A module for working with data.\n\n@docs identity, Color, Point, (+)"
                    , unions =
                        [ { name = "Color"
                          , comment = "Represents a color."
                          , args = []
                          , tags =
                                [ ( "Red", [] )
                                , ( "Green", [] )
                                , ( "Blue", [] )
                                ]
                          }
                        ]
                    , aliases =
                        [ { name = "Point"
                          , comment = "A two-dimensional point."
                          , args = []
                          , tipe =
                                Type.Record
                                    [ ( "x", Type.Type "Basics.Float" [] )
                                    , ( "y", Type.Type "Basics.Float" [] )
                                    ]
                                    Nothing
                          }
                        ]
                    , values =
                        [ { name = "identity"
                          , comment = "Return the argument unchanged.\n\n    identity 5 == 5\n    identity \"hello\" == \"hello\""
                          , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                          }
                        ]
                    , binops =
                        [ { name = "+"
                          , comment = "Add two numbers."
                          , tipe =
                                Type.Lambda (Type.Var "number")
                                    (Type.Lambda (Type.Var "number") (Type.Var "number"))
                          , associativity = Docs.Left
                          , precedence = 6
                          }
                        ]
                    }
        , Snapshot.test "module with only markdown intro" <|
            \() ->
                Render.renderModule
                    { name = "Guide"
                    , comment = "# Getting Started\n\nThis module helps you get started.\n\n## Installation\n\nRun the install command."
                    , unions = []
                    , aliases = []
                    , values = []
                    , binops = []
                    }
        ]


typeSignatureSnapshots : Snapshot.Test
typeSignatureSnapshots =
    Snapshot.describe "Type signatures"
        [ Snapshot.test "collection of complex types" <|
            \() ->
                [ ( "simple", Type.Type "Basics.Int" [] )
                , ( "function", Type.Lambda (Type.Var "a") (Type.Var "b") )
                , ( "multi-arg"
                  , Type.Lambda (Type.Type "String.String" [])
                        (Type.Lambda (Type.Type "Basics.Int" []) (Type.Type "Basics.Bool" []))
                  )
                , ( "parameterized", Type.Type "Maybe.Maybe" [ Type.Var "a" ] )
                , ( "nested", Type.Type "List.List" [ Type.Type "Maybe.Maybe" [ Type.Var "a" ] ] )
                , ( "record"
                  , Type.Record
                        [ ( "x", Type.Type "Basics.Float" [] )
                        , ( "y", Type.Type "Basics.Float" [] )
                        ]
                        Nothing
                  )
                , ( "extensible record"
                  , Type.Record [ ( "x", Type.Type "Basics.Int" [] ) ] (Just "a")
                  )
                , ( "tuple", Type.Tuple [ Type.Var "a", Type.Var "b" ] )
                , ( "unit", Type.Tuple [] )
                , ( "complex nested"
                  , Type.Type "Result.Result"
                        [ Type.Type "List.List" [ Type.Type "String.String" [] ]
                        , Type.Type "Maybe.Maybe" [ Type.Type "Basics.Int" [] ]
                        ]
                  )
                , ( "lambda in app context"
                  , Type.Type "List.List" [ Type.Lambda (Type.Var "a") (Type.Var "b") ]
                  )
                , ( "deeply nested"
                  , Type.Lambda
                        (Type.Lambda (Type.Var "a") (Type.Var "b"))
                        (Type.Lambda
                            (Type.Type "List.List" [ Type.Var "a" ])
                            (Type.Type "List.List" [ Type.Var "b" ])
                        )
                  )
                ]
                    |> List.map (\( label, tipe ) -> label ++ ": " ++ Render.typeToString tipe)
                    |> String.join "\n"
        ]


codeExampleSnapshots : Snapshot.Test
codeExampleSnapshots =
    Snapshot.describe "Code examples"
        [ Snapshot.test "value with code examples in doc comment" <|
            \() ->
                Render.renderValue
                    { name = "map"
                    , comment = "Apply a function to every element of a list.\n\n    map sqrt [1, 4, 9]  ==  [1, 2, 3]\n\n    map not [True, False]  ==  [False, True]\n\nSo `map func [ a, b, c ]` is the same as\n`[ func a, func b, func c ]`"
                    , tipe =
                        Type.Lambda
                            (Type.Lambda (Type.Var "a") (Type.Var "b"))
                            (Type.Lambda
                                (Type.Type "List.List" [ Type.Var "a" ])
                                (Type.Type "List.List" [ Type.Var "b" ])
                            )
                    }
        , Snapshot.test "union type with complex constructors" <|
            \() ->
                Render.renderUnion
                    { name = "Msg"
                    , comment = "Messages for the application."
                    , args = []
                    , tags =
                        [ ( "GotResponse", [ Type.Type "Result.Result" [ Type.Type "Http.Error" [], Type.Type "String.String" [] ] ] )
                        , ( "ClickedButton", [ Type.Type "String.String" [] ] )
                        , ( "NoOp", [] )
                        ]
                    }
        ]


multiLineTypeSnapshots : Snapshot.Test
multiLineTypeSnapshots =
    Snapshot.describe "Multi-line types"
        [ Snapshot.test "long function signature wraps at arrows" <|
            \() ->
                Render.renderValue
                    { name = "makeInfo"
                    , comment = "Build info."
                    , tipe =
                        Type.Lambda (Type.Type "String.String" [])
                            (Type.Lambda (Type.Type "String.String" [])
                                (Type.Lambda (Type.Type "Maybe.Maybe" [ Type.Type "Elm.Version.Version" [] ])
                                    (Type.Lambda (Type.Type "Maybe.Maybe" [ Type.Type "String.String" [] ])
                                        (Type.Lambda (Type.Type "String.String" [])
                                            (Type.Lambda (Type.Type "List.List" [ Type.Type "Elm.Docs.Module" [] ])
                                                (Type.Type "Info.Info" [])
                                            )
                                        )
                                    )
                                )
                            )
                    }
        , Snapshot.test "record type alias breaks per field" <|
            \() ->
                Render.renderAlias
                    { name = "Info"
                    , comment = "Info record."
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
        , Snapshot.test "function with record arg wraps" <|
            \() ->
                Render.renderValue
                    { name = "generateWithJournals"
                    , comment = "Generate output."
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
        ]


snapshotDiff : Diff.ApiDiff
snapshotDiff =
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
    }


diffSnapshots : Snapshot.Test
diffSnapshots =
    Snapshot.describe "Diff"
        [ Snapshot.test "MAJOR magnitude banner" <|
            \() ->
                Render.magnitudeBanner "MAJOR"
        , Snapshot.test "MINOR magnitude banner" <|
            \() ->
                Render.magnitudeBanner "MINOR"
        , Snapshot.test "PATCH magnitude banner" <|
            \() ->
                Render.magnitudeBanner "PATCH"
        , Snapshot.test "TOC with MINOR diff" <|
            \() ->
                Render.renderTocWithDiff snapshotDiff
                    [ { name = "NewModule"
                      , comment = "A newly added module.\n\n@docs something"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    , { name = "ChangedModule"
                      , comment = "This module has changes.\n\n@docs map, newHelper"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    , { name = "UnchangedModule"
                      , comment = "No changes here.\n\n@docs stable"
                      , unions = []
                      , aliases = []
                      , values = []
                      , binops = []
                      }
                    ]
        , Snapshot.test "module view with mixed diff items" <|
            \() ->
                Render.renderModuleWithDiff snapshotDiff
                    { name = "ChangedModule"
                    , comment = "A module with changes.\n\n@docs map, newHelper, stable"
                    , unions = []
                    , aliases = []
                    , values =
                        [ { name = "map"
                          , comment = "Map over things."
                          , tipe =
                                Type.Lambda
                                    (Type.Lambda (Type.Var "a") (Type.Var "b"))
                                    (Type.Lambda
                                        (Type.Type "List.List" [ Type.Var "a" ])
                                        (Type.Type "List.List" [ Type.Var "b" ])
                                    )
                          }
                        , { name = "newHelper"
                          , comment = "A new helper function."
                          , tipe = Type.Lambda (Type.Type "String.String" []) (Type.Type "Basics.Int" [])
                          }
                        , { name = "stable"
                          , comment = "An unchanged function."
                          , tipe = Type.Lambda (Type.Var "a") (Type.Var "a")
                          }
                        ]
                    , binops = []
                    }
        ]
