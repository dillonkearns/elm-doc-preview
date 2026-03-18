module DocsTuiTest exposing (suite)

import Dict
import DocsTui
import Docs.Diff exposing (ApiDiff)
import Elm.Docs as Docs
import Elm.Type as Type
import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Tui
import Tui.Test as TuiTest


suite : Test
suite =
    describe "DocsTui - Unified browse + diff viewer"
        [ describe "browse mode (no diff data)"
            [ test "shows module list without diff badges" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewHas "Json.Decode"
                        |> TuiTest.ensureViewDoesNotHave " + "
                        |> TuiTest.ensureViewDoesNotHave " ~ "
                        |> TuiTest.expectRunning
            , test "right pane shows full docs for selected module" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.ensureViewHas "String -> Decoder"
                        |> TuiTest.expectRunning
            , test "d key does nothing in browse mode" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewDoesNotHave "CHANGE"
                        |> TuiTest.expectRunning
            , test "navigating between modules updates docs pane" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.ensureViewHas "get"
                        |> TuiTest.expectRunning
            , test "right pane title says Docs in browse mode" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "Docs"
                        |> TuiTest.expectRunning
            , test "shows module count" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "[2]"
                        |> TuiTest.expectRunning
            , test "shows footer with selection position" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "1 of 2"
                        |> TuiTest.expectRunning
            , test "shows doc comments for values" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "Decode a JSON string"
                        |> TuiTest.expectRunning
            ]
        , describe "diff mode (with diff data)"
            [ test "d toggles to diff view when diff data present" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "MINOR CHANGE"
                        |> TuiTest.expectRunning
            , test "d toggles back to docs view" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewDoesNotHave "CHANGE"
                        |> TuiTest.ensureViewHas "Docs"
                        |> TuiTest.expectRunning
            , test "starts in docs view by default even with diff data" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "Docs"
                        |> TuiTest.ensureViewDoesNotHave "CHANGE"
                        |> TuiTest.expectRunning
            , test "diff view shows diff badges in left pane" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas " + "
                        |> TuiTest.expectRunning
            , test "docs view hides diff badges" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.ensureViewDoesNotHave " + "
                        |> TuiTest.ensureViewDoesNotHave " ~ "
                        |> TuiTest.expectRunning
            ]
        , describe "left panel tabs"
            [ test "pressing 2 switches to Changes tab when diff present" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "Changes"
                        |> TuiTest.expectRunning
            , test "pressing 1 switches back to Modules tab" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.pressKey '1'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            , test "Tab cycles through tabs when diff present" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        |> TuiTest.ensureViewHas "Changes"
                        |> TuiTest.expectRunning
            , test "pressing 2 without diff data does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewDoesNotHave "Changes"
                        |> TuiTest.expectRunning
            , test "Changes tab shows only changed modules" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "Json.Decode"
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.expectRunning
            , test "Changes tab shows diff badges" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas " + "
                        |> TuiTest.expectRunning
            ]
        , describe "inline filter"
            [ test "/ activates filter mode" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.ensureViewHas "/"
                        |> TuiTest.expectRunning
            , test "typing filters module list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.pressKey 't'
                        |> TuiTest.pressKey 't'
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.ensureViewDoesNotHave "Json"
                        |> TuiTest.expectRunning
            , test "Escape cancels filter and restores full list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.ensureViewHas "Json"
                        |> TuiTest.expectRunning
            , test "filter is case insensitive" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.pressKey 'J'
                        |> TuiTest.pressKey 'S'
                        |> TuiTest.ensureViewHas "Json"
                        |> TuiTest.expectRunning
            , test "filter shows match text in footer area" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.ensureViewHas "/h"
                        |> TuiTest.expectRunning
            ]
        , describe "navigation"
            [ test "j moves selection down" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            , test "k moves selection up" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'k'
                        |> TuiTest.ensureViewHas "1 of 2"
                        |> TuiTest.expectRunning
            , test "q exits" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'q'
                        |> TuiTest.expectExit
            ]
        , describe "help screen"
            [ test "? shows help" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Quit"
                        |> TuiTest.expectRunning
            , test "? toggles help off" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '?'
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            ]
        , describe "module tree"
            [ test "shows expand arrow for grouped modules" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.ensureViewHas "▾ Json"
                        |> TuiTest.expectRunning
            , test "Enter collapses expanded group" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.ensureViewHas "▸ Json"
                        |> TuiTest.ensureViewDoesNotHave "Json.Decode"
                        |> TuiTest.expectRunning
            , test "Enter on collapsed group expands it" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.ensureViewHas "▾ Json"
                        |> TuiTest.ensureViewHas "Json.Decode"
                        |> TuiTest.expectRunning
            , test "single-module namespace has no group arrow" <|
                \() ->
                    startBrowse [ httpModule ]
                        |> TuiTest.ensureViewDoesNotHave "▾"
                        |> TuiTest.ensureViewDoesNotHave "▸"
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.expectRunning
            , test "grouped children are indented" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.ensureViewHas "  Json.Decode"
                        |> TuiTest.expectRunning
            , test "selecting a leaf module shows its docs" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey 'j'
                        -- Navigate to Json.Decode (first child)
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            , test "selecting a group header does not show docs" <|
                \() ->
                    startBrowse treeModules
                        -- First entry is the Json group header
                        |> TuiTest.ensureViewDoesNotHave "decodeString"
                        |> TuiTest.expectRunning
            ]
        , describe "status bar"
            [ test "shows navigation hints" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "j/k"
                        |> TuiTest.ensureViewHas "quit"
                        |> TuiTest.expectRunning
            , test "shows filter hint" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "/filter"
                        |> TuiTest.expectRunning
            , test "shows Esc hint in filter mode" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '/'
                        |> TuiTest.ensureViewHas "Esc"
                        |> TuiTest.expectRunning
            , test "shows d toggle hint when diff present" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "d diff"
                        |> TuiTest.expectRunning
            , test "no d hint when no diff data" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewDoesNotHave "d diff"
                        |> TuiTest.expectRunning
            ]
        , describe "pane focus"
            [ test "l focuses right pane" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'j'
                        -- j now scrolls docs pane instead of navigating modules
                        |> TuiTest.ensureViewHas "1 of 2"
                        |> TuiTest.expectRunning
            , test "h focuses modules pane" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            ]
        ]



-- TEST HELPERS


startBrowse : List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startBrowse modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data = { modules = modules, diff = Nothing }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


startWithDiff : ApiDiff -> List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startWithDiff diff modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data = { modules = modules, diff = Just diff }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }



-- MODULE FIXTURES


jsonDecodeModule : Docs.Module
jsonDecodeModule =
    { name = "Json.Decode"
    , comment = "Decode JSON values.\n\n@docs decodeString"
    , unions = []
    , aliases = []
    , values =
        [ { name = "decodeString"
          , comment = "Decode a JSON string."
          , tipe =
                Type.Lambda
                    (Type.Type "String" [])
                    (Type.Lambda
                        (Type.Type "Decoder" [ Type.Var "a" ])
                        (Type.Type "Result" [ Type.Type "String" [], Type.Var "a" ])
                    )
          }
        ]
    , binops = []
    }


httpModule : Docs.Module
httpModule =
    { name = "Http"
    , comment = "HTTP requests.\n\n@docs get"
    , unions = []
    , aliases = []
    , values =
        [ { name = "get"
          , comment = "Make a GET request."
          , tipe =
                Type.Lambda
                    (Type.Type "String" [])
                    (Type.Type "Request" [ Type.Var "a" ])
          }
        ]
    , binops = []
    }


sampleModules : List Docs.Module
sampleModules =
    [ jsonDecodeModule, httpModule ]


jsonEncodeModule : Docs.Module
jsonEncodeModule =
    { name = "Json.Encode"
    , comment = "Encode JSON values.\n\n@docs encode"
    , unions = []
    , aliases = []
    , values =
        [ { name = "encode"
          , comment = "Encode a value."
          , tipe =
                Type.Lambda
                    (Type.Type "Int" [])
                    (Type.Lambda
                        (Type.Type "Value" [])
                        (Type.Type "String" [])
                    )
          }
        ]
    , binops = []
    }


treeModules : List Docs.Module
treeModules =
    [ jsonDecodeModule, jsonEncodeModule, httpModule ]



-- DIFF FIXTURES


sampleDiff : ApiDiff
sampleDiff =
    { magnitude = "MINOR"
    , addedModules = [ "Http" ]
    , removedModules = []
    , changedModules =
        [ { name = "Json.Decode"
          , added = [ "decodeString" ]
          , changed = []
          , removed = []
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }
