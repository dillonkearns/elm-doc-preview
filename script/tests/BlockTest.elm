module BlockTest exposing (suite)

import Docs.Block as Block
import Elm.Docs as Docs
import Elm.Type as Type
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Docs.Block.toBlocks"
        [ test "parses @docs directives into value blocks with surrounding markdown" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Some intro\n\n@docs map, filter\n\nMore text"
                        , unions = []
                        , aliases = []
                        , values =
                            [ { name = "map"
                              , comment = "Map docs"
                              , tipe = Type.Var "a"
                              }
                            , { name = "filter"
                              , comment = "Filter docs"
                              , tipe = Type.Var "a"
                              }
                            ]
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Some intro\n"
                        , Docs.ValueBlock { name = "map", comment = "Map docs", tipe = Type.Var "a" }
                        , Docs.ValueBlock { name = "filter", comment = "Filter docs", tipe = Type.Var "a" }
                        , Docs.MarkdownBlock "\n\nMore text"
                        ]
        , test "module with only markdown (no @docs) returns single MarkdownBlock" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Just some documentation with no @docs directives."
                        , unions = []
                        , aliases = []
                        , values = []
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Just some documentation with no @docs directives." ]
        , test "@docs referencing a union type produces UnionBlock" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Colors\n\n@docs Color"
                        , unions =
                            [ { name = "Color"
                              , comment = "A color"
                              , args = []
                              , tags = [ ( "Red", [] ), ( "Blue", [] ) ]
                              }
                            ]
                        , aliases = []
                        , values = []
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Colors\n"
                        , Docs.UnionBlock
                            { name = "Color"
                            , comment = "A color"
                            , args = []
                            , tags = [ ( "Red", [] ), ( "Blue", [] ) ]
                            }
                        ]
        , test "@docs referencing a type alias produces AliasBlock" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Types\n\n@docs Point"
                        , unions = []
                        , aliases =
                            [ { name = "Point"
                              , comment = "A point"
                              , args = []
                              , tipe = Type.Record [ ( "x", Type.Type "Basics.Float" [] ), ( "y", Type.Type "Basics.Float" [] ) ] Nothing
                              }
                            ]
                        , values = []
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Types\n"
                        , Docs.AliasBlock
                            { name = "Point"
                            , comment = "A point"
                            , args = []
                            , tipe = Type.Record [ ( "x", Type.Type "Basics.Float" [] ), ( "y", Type.Type "Basics.Float" [] ) ] Nothing
                            }
                        ]
        , test "@docs referencing unknown name produces UnknownBlock" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Stuff\n\n@docs nonExistent"
                        , unions = []
                        , aliases = []
                        , values = []
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Stuff\n"
                        , Docs.UnknownBlock "nonExistent"
                        ]
        , test "multiple @docs lines with mixed types" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Intro\n\n@docs map\n\n@docs Color\n\n@docs Point"
                        , unions =
                            [ { name = "Color"
                              , comment = "A color"
                              , args = []
                              , tags = [ ( "Red", [] ) ]
                              }
                            ]
                        , aliases =
                            [ { name = "Point"
                              , comment = "A point"
                              , args = []
                              , tipe = Type.Record [ ( "x", Type.Type "Basics.Float" [] ) ] Nothing
                              }
                            ]
                        , values =
                            [ { name = "map"
                              , comment = "Map docs"
                              , tipe = Type.Var "a"
                              }
                            ]
                        , binops = []
                        }
                in
                Block.toBlocks module_
                    |> List.length
                    |> Expect.equal 4
        , test "@docs with operator syntax produces BinopBlock" <|
            \() ->
                let
                    module_ =
                        { name = "MyModule"
                        , comment = "Operators\n\n@docs (+)"
                        , unions = []
                        , aliases = []
                        , values = []
                        , binops =
                            [ { name = "+"
                              , comment = "Addition"
                              , tipe = Type.Lambda (Type.Var "number") (Type.Lambda (Type.Var "number") (Type.Var "number"))
                              , associativity = Docs.Left
                              , precedence = 6
                              }
                            ]
                        }
                in
                Block.toBlocks module_
                    |> Expect.equal
                        [ Docs.MarkdownBlock "Operators\n"
                        , Docs.BinopBlock
                            { name = "+"
                            , comment = "Addition"
                            , tipe = Type.Lambda (Type.Var "number") (Type.Lambda (Type.Var "number") (Type.Var "number"))
                            , associativity = Docs.Left
                            , precedence = 6
                            }
                        ]
        ]
