module SearchEntryTest exposing (suite)

import Elm.Version as V
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Search.Entry as Entry
import Test exposing (..)


suite : Test
suite =
    describe "Entry.decoder"
        [ test "decodes package.elm-lang.org format (singular version)" <|
            \_ ->
                let
                    json =
                        Encode.list identity
                            [ Encode.object
                                [ ( "name", Encode.string "elm/browser" )
                                , ( "summary", Encode.string "Run Elm in browsers, with access to browser history for single-page apps (SPAs)" )
                                , ( "license", Encode.string "BSD-3-Clause" )
                                , ( "version", Encode.string "1.0.2" )
                                ]
                            , Encode.object
                                [ ( "name", Encode.string "elm/core" )
                                , ( "summary", Encode.string "Elm's standard libraries" )
                                , ( "license", Encode.string "BSD-3-Clause" )
                                , ( "version", Encode.string "1.0.5" )
                                ]
                            ]
                in
                case Decode.decodeValue (Decode.list Entry.decoder) json of
                    Ok entries ->
                        Expect.all
                            [ \es -> Expect.equal 2 (List.length es)
                            , \es ->
                                case es of
                                    first :: _ ->
                                        Expect.all
                                            [ \e -> Expect.equal "elm/browser" e.name
                                            , \e -> Expect.equal "elm" e.author
                                            , \e -> Expect.equal "browser" e.project
                                            , \e -> Expect.equal "BSD-3-Clause" e.license
                                            , \e -> Expect.equal [ "1.0.2" ] (List.map V.toString e.versions)
                                            ]
                                            first

                                    [] ->
                                        Expect.fail "Expected non-empty list"
                            ]
                            entries

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes local server format (versions list)" <|
            \_ ->
                let
                    json =
                        Encode.object
                            [ ( "name", Encode.string "dillonkearns/elm-pages" )
                            , ( "summary", Encode.string "A statically typed site generator" )
                            , ( "license", Encode.string "BSD-3-Clause" )
                            , ( "versions", Encode.list Encode.string [ "1.0.0", "2.0.0", "3.0.0" ] )
                            ]
                in
                case Decode.decodeValue Entry.decoder json of
                    Ok entry ->
                        Expect.all
                            [ \e -> Expect.equal "dillonkearns/elm-pages" e.name
                            , \e -> Expect.equal "dillonkearns" e.author
                            , \e -> Expect.equal "elm-pages" e.project
                            , \e -> Expect.equal [ "1.0.0", "2.0.0", "3.0.0" ] (List.map V.toString e.versions)
                            ]
                            entry

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
