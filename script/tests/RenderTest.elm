module RenderTest exposing (suite)

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
