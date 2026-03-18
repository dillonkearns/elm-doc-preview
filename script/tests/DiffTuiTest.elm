module DiffTuiTest exposing (suite)

import Dict
import DiffTui
import Docs.Diff exposing (ApiDiff, ModuleChanges)
import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Tui
import Tui.Test as TuiTest


suite : Test
suite =
    describe "DiffTui - Lazygit-style diff viewer"
        [ describe "initial view"
            [ test "shows module names in left pane" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewHas "MyModule"
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "shows diff pane on the right" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "Diff"
                        |> TuiTest.expectRunning
            , test "first module is selected by default" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "shows added items in diff pane for new module" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "New module"
                        |> TuiTest.expectRunning
            ]
        , describe "navigation"
            [ test "j moves selection down" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "MyModule"
                        |> TuiTest.expectRunning
            , test "k moves selection up" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'k'
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "arrow keys navigate" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKeyWith { key = Tui.Arrow Tui.Down, modifiers = [] }
                        |> TuiTest.pressKeyWith { key = Tui.Arrow Tui.Up, modifiers = [] }
                        |> TuiTest.expectRunning
            ]
        , describe "pane focus"
            [ test "l focuses diff pane" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.ensureViewHas "Diff"
                        |> TuiTest.expectRunning
            , test "h focuses modules pane" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.expectRunning
            ]
        , describe "quit"
            [ test "q exits" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey 'q'
                        |> TuiTest.expectExit
            , test "Escape exits" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }
                        |> TuiTest.expectExit
            ]
        , describe "diff content"
            [ test "changed module shows added items" <|
                \() ->
                    startWithDiff changedModuleDiff
                        |> TuiTest.ensureViewHas "Added"
                        |> TuiTest.ensureViewHas "newFunction"
                        |> TuiTest.expectRunning
            , test "changed module shows removed items" <|
                \() ->
                    startWithDiff changedModuleDiff
                        |> TuiTest.ensureViewHas "Removed"
                        |> TuiTest.ensureViewHas "oldFunction"
                        |> TuiTest.expectRunning
            , test "changed module shows changed items" <|
                \() ->
                    startWithDiff changedModuleDiff
                        |> TuiTest.ensureViewHas "Changed"
                        |> TuiTest.ensureViewHas "modifiedFunction"
                        |> TuiTest.expectRunning
            , test "removed module shows removal message" <|
                \() ->
                    startWithDiff removedModuleDiff
                        |> TuiTest.ensureViewHas "Removed module"
                        |> TuiTest.expectRunning
            ]
        , describe "diff pane scrolling"
            [ test "j/k scrolls when diff pane is focused" <|
                \() ->
                    startWithDiff changedModuleDiff
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.expectRunning
            ]
        , describe "module footer"
            [ test "shows selection position in footer" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "1 of"
                        |> TuiTest.expectRunning
            ]
        , describe "navigating between modules updates diff"
            [ test "selecting second module shows its diff" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "New module"
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "Added"
                        |> TuiTest.ensureViewHas "identity"
                        |> TuiTest.ensureViewDoesNotHave "New module"
                        |> TuiTest.expectRunning
            ]
        , describe "magnitude banner"
            [ test "shows magnitude in the diff view" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.ensureViewHas "MINOR"
                        |> TuiTest.expectRunning
            ]
        , describe "README diff"
            [ test "shows README tab when readme diff exists" <|
                \() ->
                    startWithDiff readmeDiff
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.expectRunning
            , test "selecting README shows diff content" <|
                \() ->
                    startWithDiff readmeDiff
                        |> TuiTest.ensureViewHas "+ New content"
                        |> TuiTest.expectRunning
            ]
        , describe "comment diffs"
            [ test "shows comment diffs for items with doc changes" <|
                \() ->
                    startWithDiff commentDiff
                        |> TuiTest.ensureViewHas "Doc changes"
                        |> TuiTest.expectRunning
            ]
        , describe "combined diff"
            [ test "shows all types of changes together" <|
                \() ->
                    startWithDiff fullDiff
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.ensureViewHas "ChangedModule"
                        |> TuiTest.ensureViewHas "AddedModule"
                        |> TuiTest.ensureViewHas "RemovedModule"
                        |> TuiTest.expectRunning
            , test "module count badge is correct" <|
                \() ->
                    startWithDiff fullDiff
                        |> TuiTest.ensureViewHas "[4]"
                        |> TuiTest.expectRunning
            ]
        , describe "empty diff"
            [ test "handles diff with no changes gracefully" <|
                \() ->
                    startWithDiff emptyDiff
                        |> TuiTest.ensureViewHas "PATCH"
                        |> TuiTest.expectRunning
            ]
        , describe "help screen"
            [ test "? shows help" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Quit"
                        |> TuiTest.ensureViewHas "Next module"
                        |> TuiTest.expectRunning
            , test "? toggles help off" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey '?'
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            , test "Escape closes help" <|
                \() ->
                    startWithDiff sampleDiff
                        |> TuiTest.pressKey '?'
                        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            ]
        , describe "snapshots"
            [ test "captures snapshots for stepper visualization" <|
                \() ->
                    let
                        snapshots =
                            startWithDiff sampleDiff
                                |> TuiTest.pressKey 'j'
                                |> TuiTest.pressKey 'l'
                                |> TuiTest.pressKey 'j'
                                |> TuiTest.toSnapshots
                    in
                    Expect.equal 4 (List.length snapshots)
            ]
        ]



-- TEST HELPERS


startWithDiff : ApiDiff -> TuiTest.TuiTest DiffTui.Model DiffTui.Msg
startWithDiff diff =
    TuiTest.start
        { data = diff
        , init = DiffTui.init
        , update = DiffTui.update
        , view = DiffTui.view
        , subscriptions = DiffTui.subscriptions
        }



-- FIXTURES


sampleDiff : ApiDiff
sampleDiff =
    { magnitude = "MINOR"
    , addedModules = [ "NewModule" ]
    , removedModules = []
    , changedModules =
        [ { name = "MyModule"
          , added = [ "identity" ]
          , changed = []
          , removed = []
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


changedModuleDiff : ApiDiff
changedModuleDiff =
    { magnitude = "MAJOR"
    , addedModules = []
    , removedModules = []
    , changedModules =
        [ { name = "Api.Module"
          , added = [ "newFunction" ]
          , changed = [ "modifiedFunction" ]
          , removed = [ "oldFunction" ]
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


removedModuleDiff : ApiDiff
removedModuleDiff =
    { magnitude = "MAJOR"
    , addedModules = []
    , removedModules = [ "OldModule" ]
    , changedModules = []
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


readmeDiff : ApiDiff
readmeDiff =
    { magnitude = "PATCH"
    , addedModules = []
    , removedModules = []
    , changedModules =
        [ { name = "MyModule"
          , added = []
          , changed = []
          , removed = []
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Just "- Old content\n+ New content"
    , oldTypes = Dict.empty
    }


fullDiff : ApiDiff
fullDiff =
    { magnitude = "MAJOR"
    , addedModules = [ "AddedModule" ]
    , removedModules = [ "RemovedModule" ]
    , changedModules =
        [ { name = "ChangedModule"
          , added = [ "newFn" ]
          , changed = [ "updatedFn" ]
          , removed = [ "deletedFn" ]
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Just "- old readme\n+ new readme"
    , oldTypes = Dict.empty
    }


emptyDiff : ApiDiff
emptyDiff =
    { magnitude = "PATCH"
    , addedModules = []
    , removedModules = []
    , changedModules = []
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


commentDiff : ApiDiff
commentDiff =
    { magnitude = "PATCH"
    , addedModules = []
    , removedModules = []
    , changedModules =
        [ { name = "MyModule"
          , added = []
          , changed = []
          , removed = []
          }
        ]
    , commentDiffs =
        Dict.fromList
            [ ( "MyModule"
              , Dict.fromList
                    [ ( "myFunction", "- Old docs\n+ New docs" )
                    ]
              )
            ]
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }
