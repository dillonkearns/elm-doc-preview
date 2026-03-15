module Snapshots exposing (run)

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
