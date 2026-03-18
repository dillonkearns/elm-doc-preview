module DiffTuiTest exposing (suite)

import Dict
import DiffTui
import Docs.Diff exposing (ApiDiff, ModuleChanges)
import Elm.Docs as Docs
import Elm.Type as Type
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
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewHas "MyModule"
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "shows diff pane on the right" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "Diff"
                        |> TuiTest.expectRunning
            , test "first module is selected by default" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "shows added items in diff pane for new module" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "New module"
                        |> TuiTest.expectRunning
            ]
        , describe "navigation"
            [ test "j moves selection down" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "MyModule"
                        |> TuiTest.expectRunning
            , test "k moves selection up" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'k'
                        |> TuiTest.ensureViewHas "NewModule"
                        |> TuiTest.expectRunning
            , test "arrow keys navigate" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Arrow Tui.Down, modifiers = [] }
                        |> TuiTest.pressKeyWith { key = Tui.Arrow Tui.Up, modifiers = [] }
                        |> TuiTest.expectRunning
            ]
        , describe "pane focus"
            [ test "l focuses diff pane" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.ensureViewHas "Diff"
                        |> TuiTest.expectRunning
            , test "h focuses modules pane" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'h'
                        |> TuiTest.expectRunning
            ]
        , describe "quit"
            [ test "q exits" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'q'
                        |> TuiTest.expectExit
            , test "Escape exits" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }
                        |> TuiTest.expectExit
            ]
        , describe "rendered docs - type signatures"
            [ test "shows type signature for added value" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'j'
                        -- MyModule is second, has added "identity : a -> a"
                        |> TuiTest.ensureViewHas "identity"
                        |> TuiTest.ensureViewHas "a -> a"
                        |> TuiTest.expectRunning
            , test "shows type signature for changed value" <|
                \() ->
                    startWith changedModuleDiff changedModules
                        |> TuiTest.ensureViewHas "modifiedFunction"
                        |> TuiTest.ensureViewHas "String -> Int"
                        |> TuiTest.expectRunning
            , test "shows union type constructors" <|
                \() ->
                    startWith unionDiff unionModules
                        |> TuiTest.ensureViewHas "type Status"
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.ensureViewHas "Success"
                        |> TuiTest.expectRunning
            , test "shows type alias definition" <|
                \() ->
                    startWith aliasDiff aliasModules
                        |> TuiTest.ensureViewHas "type alias Config"
                        |> TuiTest.expectRunning
            ]
        , describe "rendered docs - doc comments"
            [ test "shows doc comment for value" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "Return the argument unchanged"
                        |> TuiTest.expectRunning
            ]
        , describe "rendered docs - old type signatures"
            [ test "shows old type signature for changed items" <|
                \() ->
                    startWith changedWithOldType changedModules
                        |> TuiTest.ensureViewHas "String -> String"
                        |> TuiTest.ensureViewHas "String -> Int"
                        |> TuiTest.expectRunning
            ]
        , describe "diff content"
            [ test "changed module shows removed items" <|
                \() ->
                    startWith changedModuleDiff changedModules
                        |> TuiTest.ensureViewHas "oldFunction"
                        |> TuiTest.expectRunning
            , test "removed module shows removal message" <|
                \() ->
                    startWith removedModuleDiff []
                        |> TuiTest.ensureViewHas "Removed module"
                        |> TuiTest.expectRunning
            ]
        , describe "diff pane scrolling"
            [ test "j/k scrolls when diff pane is focused" <|
                \() ->
                    startWith changedModuleDiff changedModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.expectRunning
            ]
        , describe "module footer"
            [ test "shows selection position in footer" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "1 of"
                        |> TuiTest.expectRunning
            ]
        , describe "navigating between modules updates diff"
            [ test "selecting second module shows its diff" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "New module"
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "identity"
                        |> TuiTest.ensureViewHas "a -> a"
                        |> TuiTest.ensureViewDoesNotHave "New module"
                        |> TuiTest.expectRunning
            ]
        , describe "magnitude banner"
            [ test "shows magnitude in the diff view" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.ensureViewHas "MINOR"
                        |> TuiTest.expectRunning
            ]
        , describe "README diff"
            [ test "shows README tab when readme diff exists" <|
                \() ->
                    startWith readmeDiff readmeModules
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.expectRunning
            , test "selecting README shows diff content" <|
                \() ->
                    startWith readmeDiff readmeModules
                        |> TuiTest.ensureViewHas "+ New content"
                        |> TuiTest.expectRunning
            ]
        , describe "comment diffs"
            [ test "shows comment diffs for items with doc changes" <|
                \() ->
                    startWith commentDiff commentModules
                        |> TuiTest.ensureViewHas "Doc changes"
                        |> TuiTest.expectRunning
            ]
        , describe "combined diff"
            [ test "shows all types of changes together" <|
                \() ->
                    startWith fullDiff fullModules
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.ensureViewHas "ChangedModule"
                        |> TuiTest.ensureViewHas "AddedModule"
                        |> TuiTest.ensureViewHas "RemovedModule"
                        |> TuiTest.expectRunning
            , test "module count badge is correct" <|
                \() ->
                    startWith fullDiff fullModules
                        |> TuiTest.ensureViewHas "[4]"
                        |> TuiTest.expectRunning
            ]
        , describe "empty diff"
            [ test "handles diff with no changes gracefully" <|
                \() ->
                    startWith emptyDiff []
                        |> TuiTest.ensureViewHas "PATCH"
                        |> TuiTest.expectRunning
            ]
        , describe "help screen"
            [ test "? shows help" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Quit"
                        |> TuiTest.ensureViewHas "Next module"
                        |> TuiTest.expectRunning
            , test "? toggles help off" <|
                \() ->
                    startWith sampleDiff sampleModules
                        |> TuiTest.pressKey '?'
                        |> TuiTest.pressKey '?'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            , test "Escape closes help" <|
                \() ->
                    startWith sampleDiff sampleModules
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
                            startWith sampleDiff sampleModules
                                |> TuiTest.pressKey 'j'
                                |> TuiTest.pressKey 'l'
                                |> TuiTest.pressKey 'j'
                                |> TuiTest.toSnapshots
                    in
                    Expect.equal 4 (List.length snapshots)
            ]
        ]



-- TEST HELPERS


type alias InitData =
    { diff : ApiDiff
    , modules : List Docs.Module
    }


startWith : ApiDiff -> List Docs.Module -> TuiTest.TuiTest DiffTui.Model DiffTui.Msg
startWith diff modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data = { diff = diff, modules = modules }
        , init = DiffTui.init
        , update = DiffTui.update
        , view = DiffTui.view
        , subscriptions = DiffTui.subscriptions
        }



-- MODULE FIXTURES


myModule : Docs.Module
myModule =
    { name = "MyModule"
    , comment = "A module for doing things.\n\n@docs identity"
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


newModule : Docs.Module
newModule =
    { name = "NewModule"
    , comment = "A new module.\n\n@docs hello"
    , unions = []
    , aliases = []
    , values =
        [ { name = "hello"
          , comment = "Say hello."
          , tipe = Type.Type "String" []
          }
        ]
    , binops = []
    }


sampleModules : List Docs.Module
sampleModules =
    [ newModule, myModule ]


changedModule : Docs.Module
changedModule =
    { name = "Api.Module"
    , comment = "An API module.\n\n@docs newFunction, modifiedFunction"
    , unions = []
    , aliases = []
    , values =
        [ { name = "newFunction"
          , comment = "A new function."
          , tipe = Type.Lambda (Type.Type "String" []) (Type.Type "String" [])
          }
        , { name = "modifiedFunction"
          , comment = "A modified function."
          , tipe = Type.Lambda (Type.Type "String" []) (Type.Type "Int" [])
          }
        ]
    , binops = []
    }


changedModules : List Docs.Module
changedModules =
    [ changedModule ]


unionModule : Docs.Module
unionModule =
    { name = "Types"
    , comment = "Type definitions.\n\n@docs Status"
    , unions =
        [ { name = "Status"
          , comment = "Represents loading state."
          , args = []
          , tags =
                [ ( "Loading", [] )
                , ( "Success", [ Type.Type "String" [] ] )
                , ( "Failure", [ Type.Type "String" [] ] )
                ]
          }
        ]
    , aliases = []
    , values = []
    , binops = []
    }


unionModules : List Docs.Module
unionModules =
    [ unionModule ]


aliasModule : Docs.Module
aliasModule =
    { name = "Config"
    , comment = "Configuration.\n\n@docs Config"
    , unions = []
    , aliases =
        [ { name = "Config"
          , comment = "App configuration."
          , args = []
          , tipe =
                Type.Record
                    [ ( "host", Type.Type "String" [] )
                    , ( "port_", Type.Type "Int" [] )
                    ]
                    Nothing
          }
        ]
    , values = []
    , binops = []
    }


aliasModules : List Docs.Module
aliasModules =
    [ aliasModule ]


readmeModules : List Docs.Module
readmeModules =
    [ myModule ]


commentModules : List Docs.Module
commentModules =
    [ myModule ]


addedModule : Docs.Module
addedModule =
    { name = "AddedModule"
    , comment = "New.\n\n@docs thing"
    , unions = []
    , aliases = []
    , values =
        [ { name = "thing"
          , comment = "A thing."
          , tipe = Type.Type "Int" []
          }
        ]
    , binops = []
    }


changedModuleFull : Docs.Module
changedModuleFull =
    { name = "ChangedModule"
    , comment = "Changed.\n\n@docs newFn, updatedFn"
    , unions = []
    , aliases = []
    , values =
        [ { name = "newFn"
          , comment = "New."
          , tipe = Type.Type "Int" []
          }
        , { name = "updatedFn"
          , comment = "Updated."
          , tipe = Type.Lambda (Type.Type "String" []) (Type.Type "Int" [])
          }
        ]
    , binops = []
    }


fullModules : List Docs.Module
fullModules =
    [ addedModule, changedModuleFull ]



-- DIFF FIXTURES


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


changedWithOldType : ApiDiff
changedWithOldType =
    { magnitude = "MAJOR"
    , addedModules = []
    , removedModules = []
    , changedModules =
        [ { name = "Api.Module"
          , added = []
          , changed = [ "modifiedFunction" ]
          , removed = []
          }
        ]
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes =
        Dict.fromList
            [ ( "Api.Module"
              , Dict.fromList
                    [ ( "modifiedFunction"
                      , Encode.string "String -> String"
                      )
                    ]
              )
            ]
    }


unionDiff : ApiDiff
unionDiff =
    { magnitude = "MINOR"
    , addedModules = [ "Types" ]
    , removedModules = []
    , changedModules = []
    , commentDiffs = Dict.empty
    , readmeDiff = Nothing
    , oldTypes = Dict.empty
    }


aliasDiff : ApiDiff
aliasDiff =
    { magnitude = "MINOR"
    , addedModules = [ "Config" ]
    , removedModules = []
    , changedModules = []
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
