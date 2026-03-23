module DocsTuiTest exposing (browseMode, diffMode, loadDiffFromVersions, run, suite, versionsTab)

import BackendTask
import BackendTask.Custom
import Dict
import DocsTui
import DocsTuiApp
import Pages.Script as Script
import Docs.Diff as Diff exposing (ApiDiff)
import Elm.Docs as Docs
import Json.Decode as Decode
import Elm.Type as Type
import Expect
import FatalError exposing (FatalError)
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest
import Tui
import Tui.Test as TuiTest
import Tui.Test.Stepper


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
                        -- With tree view: ▼ Json (group) → Json.Decode → Http
                        |> TuiTest.pressKey 'j'
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
            , test "shows colored gutter marks for values" <|
                \() ->
                    startBrowse [ multiValueModule ]
                        |> TuiTest.ensureViewHas "┃ foo"
                        |> TuiTest.ensureViewHas "┃ bar"
                        |> TuiTest.expectRunning
            ]
        , describe "diff mode (with diff data)"
            [ test "d toggles to diff view with change summary" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "MINOR CHANGE"
                        |> TuiTest.ensureViewHas "added"
                        |> TuiTest.ensureViewHas "across"
                        |> TuiTest.expectRunning
            , test "d toggles back to docs view and restores modules tab" <|
                \() ->
                    -- Use treeModules which has Json.Encode — NOT in the diff
                    startWithDiff sampleDiff treeModules
                        |> TuiTest.pressKey 'd'
                        -- In diff view, Json.Encode should NOT be visible (not changed)
                        |> TuiTest.ensureViewDoesNotHave "Json.Encode"
                        |> TuiTest.pressKey 'd'
                        -- After toggling back, Json.Encode SHOULD be visible (all modules shown)
                        |> TuiTest.ensureViewHas "Json.Encode"
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
                        |> TuiTest.pressKey 'd'
                        -- Should show diff badges for changed modules
                        |> TuiTest.ensureViewHas " + "
                        |> TuiTest.expectRunning
            , test "pressing 1 switches back to Modules tab" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.pressKey '1'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.expectRunning
            , test "Shift+Tab cycles pane focus backward" <|
                \() ->
                    startBrowse sampleModules
                        -- Tab forward to items
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Shift+Tab back to modules
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [ Tui.Shift ] }
                        -- j navigates modules; need 2 j's to get past tree group to Http
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            , test "Tab cycles pane focus" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from modules to items
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from items to docs
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from docs back to modules; need 2 j's for tree
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            , test "pressing 2 without diff or versions does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewDoesNotHave "Changes"
                        |> TuiTest.expectRunning
            , test "pressing 2 available when versions exist (no diff yet)" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'd'
                        -- Should trigger diff loading
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.expectRunning
            , test "pressing 2 with versions triggers diff loading" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.ensureViewHas "MINOR CHANGE"
                        |> TuiTest.expectRunning
            , test "Changes tab shows only changed modules" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "Json.Decode"
                        |> TuiTest.ensureViewHas "Http"
                        |> TuiTest.expectRunning
            , test "Changes tab shows diff badges" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas " + "
                        |> TuiTest.expectRunning
            ]
        -- inline filter tests removed — using native Layout.selectableList filtering
        , describe "navigation"
            [ test "j moves selection down" <|
                \() ->
                    startBrowse sampleModules
                        -- 2 j's: Json group → Json.Decode → Http
                        |> TuiTest.pressKey 'j'
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
            , test "> pages down in modules list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '>'
                        |> TuiTest.ensureViewHas "3 of 3"
                        |> TuiTest.expectRunning
            , test "< pages up in modules list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '>'
                        |> TuiTest.pressKey '<'
                        |> TuiTest.ensureViewHas "1 of 3"
                        |> TuiTest.expectRunning
            , test "> scrolls docs pane when focused" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey '>'
                        -- Should scroll without changing module selection
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
                        |> TuiTest.ensureViewHas "▼ Json"
                        |> TuiTest.expectRunning
            , test "Enter collapses expanded group" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.ensureViewHas "▸ Json"
                        -- Json.Decode hidden in modules pane but may appear in docs title
                        |> TuiTest.expectRunning
            , test "Enter on collapsed group expands it" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.ensureViewHas "▼ Json"
                        |> TuiTest.ensureViewHas "Json.Decode"
                        |> TuiTest.expectRunning
            , test "single-module namespace has no group arrow" <|
                \() ->
                    startBrowse [ httpModule ]
                        |> TuiTest.ensureViewDoesNotHave "▼"
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
                        -- Navigate to Json.Decode (first child under Json group)
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            , test "selecting a group header does not show docs" <|
                \() ->
                    startBrowse treeModules
                        -- First entry is the Json group header; docs pane shows first module by default
                        -- The group itself doesn't have docs content
                        |> TuiTest.ensureViewHas "▼ Json"
                        |> TuiTest.expectRunning
            ]
        , describe "items pane"
            [ test "shows items from selected module" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "Items"
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            , test "items update when module selection changes" <|
                \() ->
                    startBrowse sampleModules
                        -- Tree: ▼ Json → Json.Decode → Http; need 2 j's to reach Http
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "get"
                        |> TuiTest.ensureViewDoesNotHave "decodeString"
                        |> TuiTest.expectRunning
            , test "in diff view, items pane shows only changed items" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        -- d switches to ChangesTab; first entry is Http (added)
                        -- Navigate to Json.Decode (second entry)
                        |> TuiTest.pressKey 'j'
                        -- sampleDiff has Json.Decode with added: ["decodeString"]
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            , test "in diff view, newly added module shows its items not another module's" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'c'
                        -- Changes tab: Http (added) is first entry, already selected
                        -- Items pane should show Http's "get", not Json.Decode's "decodeString"
                        |> TuiTest.ensureViewHas " + Http"
                        |> TuiTest.ensureViewDoesNotHave "decodeString"
                        |> TuiTest.expectRunning
            , test "scrolling docs updates items pane selection" <|
                \() ->
                    -- Use small terminal so content requires scrolling
                    TuiTest.startWithContext { width = 120, height = 8, colorProfile = Tui.TrueColor }
                        { data =
                            { modules = [ multiValueModule ]
                            , diff = Nothing
                            , versions = []
                            , loadDiff = noopLoadDiff
                            , dependencies = []
                            , loadPackageDocs = \_ -> BackendTask.succeed []
                            , readme = Nothing
                            }
                        , init = DocsTui.init
                        , update = DocsTui.update
                        , view = DocsTui.view
                        , subscriptions = DocsTui.subscriptions
                        }
                        -- Focus docs pane
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Scroll down past foo to bar
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        -- Items pane should now show "2 of 2" (bar selected)
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            , test "selecting item does not match type alias field with same name" <|
                \() ->
                    TuiTest.startWithContext { width = 100, height = 15, colorProfile = Tui.TrueColor }
                        { data =
                            { modules = [ radiusModule ]
                            , diff = Nothing
                            , versions = []
                            , loadDiff = noopLoadDiff
                            , dependencies = []
                            , loadPackageDocs = \_ -> BackendTask.succeed []
                            , readme = Nothing
                            }
                        , init = DocsTui.init
                        , update = DocsTui.update
                        , view = DocsTui.view
                        , subscriptions = DocsTui.subscriptions
                        }
                        -- Focus items pane, navigate to "xs" (past RadiusScale, Radius, # Radius, none)
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        -- xs should be selected, docs should show "xs : Radius" definition
                        -- NOT the "xs : Float" field inside RadiusScale
                        |> TuiTest.ensureViewHas "Extra small radius"
                        |> TuiTest.expectRunning
            , test "j in items pane scrolls docs to selected item" <|
                \() ->
                    TuiTest.startWithContext { width = 120, height = 8, colorProfile = Tui.TrueColor }
                        { data =
                            { modules = [ multiValueModule ]
                            , diff = Nothing
                            , versions = []
                            , loadDiff = noopLoadDiff
                            , dependencies = []
                            , loadPackageDocs = \_ -> BackendTask.succeed []
                            , readme = Nothing
                            }
                        , init = DocsTui.init
                        , update = DocsTui.update
                        , view = DocsTui.view
                        , subscriptions = DocsTui.subscriptions
                        }
                        -- Focus items pane and navigate to bar
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        |> TuiTest.pressKey 'j'
                        -- Docs should scroll to show bar near the top
                        |> TuiTest.ensureViewHas "┃ bar"
                        |> TuiTest.expectRunning
            , test "items pane shows section headings from module comment" <|
                \() ->
                    startBrowse [ sectionedModule ]
                        -- 2 values + 2 section headings = 4 items total
                        |> TuiTest.ensureViewHas "1 of 4"
                        |> TuiTest.ensureViewHas "# Basics"
                        |> TuiTest.ensureViewHas "# Advanced"
                        |> TuiTest.expectRunning
            , test "items pane shows all section headings including from same markdown block" <|
                \() ->
                    startBrowse [ cliOptionModule ]
                        -- 3 headings (When to Use, Example, Decoders) + 4 items (Option, CliDecoder, string, int) = 7
                        |> TuiTest.ensureViewHas "1 of 7"
                        |> TuiTest.ensureViewHas "# Decoders"
                        |> TuiTest.expectRunning
            , test "items pane shows items for initially selected module" <|
                \() ->
                    startBrowse treeModules
                        -- With native tree view, selectedModuleName defaults to first module
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            ]
        , describe "status bar"
            [ test "shows navigation hints" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.ensureViewHas "j/k"
                        |> TuiTest.ensureViewHas "quit"
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
                        -- 2 j's to navigate past tree group
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            ]
        , describe "versions tab"
            [ test "pressing 3 switches to Versions tab" <|
                \() ->
                    startWithVersions [ "12.1.0", "12.0.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.ensureViewHas "12.1.0"
                        |> TuiTest.ensureViewHas "12.0.0"
                        |> TuiTest.expectRunning
            , test "pressing 3 without versions does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewDoesNotHave "Versions"
                        |> TuiTest.expectRunning
            , test "Tab cycles pane focus with versions" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves to items pane
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves to docs pane
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves back to modules
                        |> TuiTest.ensureViewHas "1 of 2"
                        |> TuiTest.expectRunning
            , test "j/k navigates version list" <|
                \() ->
                    startWithVersions [ "12.1.0", "12.0.0", "11.0.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 3"
                        |> TuiTest.expectRunning
            , test "version ordering normalizes older → newer" <|
                \() ->
                    startWithVersions [ "12.1.0", "12.0.0", "11.0.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        -- Select 12.1.0 (newest), press d, pick 11.0.0 (oldest)
                        |> TuiTest.pressKey 'd'
                        -- Picker should show other versions
                        |> TuiTest.ensureViewHas "Compare 12.1.0"
                        |> TuiTest.expectRunning
            , test "d on version opens compare-against picker" <|
                \() ->
                    startWithVersions [ "12.1.0", "12.0.0", "11.0.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        -- Select 12.1.0 (first version)
                        |> TuiTest.pressKey 'd'
                        -- Should open a picker showing other versions to compare against
                        |> TuiTest.ensureViewHas "Compare 12.1.0"
                        |> TuiTest.ensureViewHas "HEAD"
                        |> TuiTest.ensureViewHas "12.0.0"
                        |> TuiTest.ensureViewHas "11.0.0"
                        |> TuiTest.expectRunning
            ]
        , describe "on-demand diff loading"
            [ test "Enter on version shows Loading with spinner" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.ensureViewHas "|"
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.expectRunning
            , test "resolving diff effect shows diff view" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.ensureViewHas "MINOR CHANGE"
                        |> TuiTest.expectRunning
            , test "diff pane title shows version" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.ensureViewHas "vs 12.1.0"
                        |> TuiTest.expectRunning
            , test "shows toast when diff finishes loading" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'v'
                        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.ensureViewHas "Diff loaded"
                        |> TuiTest.expectRunning
            ]
        , describe "d shortcut with versions"
            [ test "d with no diff but versions triggers loading" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.expectRunning
            , test "d with no diff and no versions does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewDoesNotHave "Loading"
                        |> TuiTest.expectRunning
            , test "d while already loading does nothing" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "Loading"
                        |> TuiTest.resolveEffect
                            (BackendTaskTest.simulateCustom "loadDiff"
                                (encodeApiDiff sampleDiff)
                            )
                        |> TuiTest.pressKey 'd'
                        -- After loading resolved, d toggles back to docs
                        |> TuiTest.ensureViewDoesNotHave "Loading"
                        |> TuiTest.expectRunning
            , test "d with existing diff still toggles" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewHas "MINOR CHANGE"
                        |> TuiTest.expectRunning
            ]
        , describe "versions status bar"
            [ test "status bar shows d diff when versions available" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.ensureViewHas "d diff"
                        |> TuiTest.expectRunning
            , test "status bar shows tab hints when versions available" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.ensureViewHas "1/2/3 tabs"
                        |> TuiTest.expectRunning
            ]
        , describe "readme diff"
            [ test "Changes tab shows README entry when readmeDiff exists" <|
                \() ->
                    startWithDiff sampleDiffWithReadme sampleModules
                        |> TuiTest.pressKey 'c'
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.expectRunning
            , test "selecting README shows diff lines in right pane" <|
                \() ->
                    startWithDiff sampleDiffWithReadme sampleModules
                        |> TuiTest.pressKey 'c'
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.ensureViewHas "New intro line"
                        |> TuiTest.expectRunning
            , test "internal doc links are styled distinctly" <|
                \() ->
                    startBrowse [ cliOptionModule ]
                        -- Navigate to a module that has links in its docs
                        |> TuiTest.pressKey 'j'
                        -- cliOptionModule docs contain ## headings
                        -- Internal links should be magenta+underline (not cyan like code)
                        |> TuiTest.expectRunning
            , test "README does not appear in Changes tab when no readmeDiff" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.ensureViewDoesNotHave "README"
                        |> TuiTest.expectRunning
            ]
        , describe "package picker"
            [ test "p opens package picker with dependencies" <|
                \() ->
                    startWithDeps [ "elm/json", "elm/http" ] sampleModules
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.ensureViewHas "Browse Package"
                        |> TuiTest.ensureViewHas "elm/json"
                        |> TuiTest.ensureViewHas "elm/http"
                        |> TuiTest.expectRunning
            , test "p shows all registry packages in picker" <|
                \() ->
                    startWithDeps [ "elm/json", "elm/http", "dillonkearns/elm-pages", "mdgriffith/elm-ui" ] sampleModules
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.ensureViewHas "Browse Package"
                        |> TuiTest.ensureViewHas "dillonkearns/elm-pages"
                        |> TuiTest.ensureViewHas "mdgriffith/elm-ui"
                        |> TuiTest.expectRunning
            , test "Escape closes package picker" <|
                \() ->
                    startWithDeps [ "elm/json" ] sampleModules
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }
                        |> TuiTest.ensureViewDoesNotHave "Browse Package"
                        |> TuiTest.expectRunning
            , test "space in picker search matches across word boundaries" <|
                \() ->
                    startWithDeps [ "jfmengels/elm-review", "elm/json", "elm/http" ] sampleModules
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.pressKey 'f'
                        |> TuiTest.pressKey 'm'
                        |> TuiTest.pressKey ' '
                        |> TuiTest.pressKey 'e'
                        |> TuiTest.pressKey 'l'
                        |> TuiTest.pressKey 'm'
                        |> TuiTest.pressKey '-'
                        |> TuiTest.pressKey 'r'
                        |> TuiTest.ensureViewHas "jfmengels/elm-review"
                        |> TuiTest.expectRunning
            ]
        , describe "internal link click navigation"
            [ test "clicking on link text navigates and updates docs pane" <|
                \() ->
                    startBrowse [ linkModule, httpModule ]
                        |> TuiTest.ensureViewHas "Docs: MyModule"
                        |> TuiTest.clickText "See"
                        -- Should show Http's docs content, not MyModule's
                        |> TuiTest.ensureViewHas "Docs: Http"
                        |> TuiTest.ensureViewHas "Make a GET request"
                        |> TuiTest.ensureViewDoesNotHave "A module with links"
                        |> TuiTest.expectRunning
            , test "p without dependencies does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey 'p'
                        |> TuiTest.ensureViewDoesNotHave "Browse Package"
                        |> TuiTest.expectRunning
            ]
        , describe "loadAllPackageNames"
            [ test "fetches package names from search.json" <|
                \() ->
                    DocsTuiApp.loadAllPackageNames
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withEnv "HOME" "/Users/test"
                            )
                        |> BackendTaskTest.simulateCommand "mkdir" ""
                        |> BackendTaskTest.simulateHttpGet
                            "https://package.elm-lang.org/search.json"
                            (Encode.list
                                (\( name, summary ) ->
                                    Encode.object
                                        [ ( "name", Encode.string name )
                                        , ( "summary", Encode.string summary )
                                        , ( "license", Encode.string "BSD-3-Clause" )
                                        , ( "version", Encode.string "1.0.0" )
                                        ]
                                )
                                [ ( "elm/core", "Elm core libraries" )
                                , ( "elm/json", "Encode and decode JSON" )
                                , ( "elm/http", "Make HTTP requests" )
                                ]
                            )
                        |> BackendTaskTest.expectSuccessWith
                            (Expect.equal [ "elm/core", "elm/http", "elm/json" ])
            , test "returns sorted package names" <|
                \() ->
                    DocsTuiApp.loadAllPackageNames
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withEnv "HOME" "/Users/test"
                            )
                        |> BackendTaskTest.simulateCommand "mkdir" ""
                        |> BackendTaskTest.simulateHttpGet
                            "https://package.elm-lang.org/search.json"
                            (Encode.list
                                (\name ->
                                    Encode.object
                                        [ ( "name", Encode.string name )
                                        , ( "summary", Encode.string "" )
                                        , ( "license", Encode.string "MIT" )
                                        , ( "version", Encode.string "1.0.0" )
                                        ]
                                )
                                [ "mdgriffith/elm-ui", "elm/core", "dillonkearns/elm-pages" ]
                            )
                        |> BackendTaskTest.expectSuccessWith
                            (Expect.equal [ "dillonkearns/elm-pages", "elm/core", "mdgriffith/elm-ui" ])
            ]
        ]



-- TEST HELPERS


noopLoadDiff : String -> BackendTask.BackendTask FatalError (Maybe ApiDiff)
noopLoadDiff _ =
    BackendTask.succeed Nothing


startBrowse : List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startBrowse modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data =
            { modules = modules
            , diff = Nothing
            , versions = []
            , loadDiff = noopLoadDiff
            , dependencies = []
            , loadPackageDocs = \_ -> BackendTask.succeed []
            , readme = Nothing
            }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


startWithDiff : ApiDiff -> List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startWithDiff diff modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data =
            { modules = modules
            , diff = Just diff
            , versions = []
            , loadDiff = noopLoadDiff
            , dependencies = []
            , loadPackageDocs = \_ -> BackendTask.succeed []
            , readme = Nothing
            }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


testLoadDiff : String -> BackendTask.BackendTask FatalError (Maybe ApiDiff)
testLoadDiff version =
    BackendTask.Custom.run "loadDiff"
        (Encode.string version)
        Diff.decoder
        |> BackendTask.allowFatal


startWithVersions : List String -> List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startWithVersions versions modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data =
            { modules = modules
            , diff = Nothing
            , versions = versions
            , loadDiff = testLoadDiff
            , dependencies = []
            , loadPackageDocs = \_ -> BackendTask.succeed []
            , readme = Nothing
            }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


startWithDeps : List String -> List Docs.Module -> TuiTest.TuiTest DocsTui.Model DocsTui.Msg
startWithDeps deps modules =
    TuiTest.startWithContext { width = 120, height = 40, colorProfile = Tui.TrueColor }
        { data =
            { modules = modules
            , diff = Nothing
            , versions = []
            , loadDiff = noopLoadDiff
            , dependencies = deps
            , loadPackageDocs = testLoadPackageDocs
            , readme = Nothing
            }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


testLoadPackageDocs : String -> BackendTask.BackendTask FatalError (List Docs.Module)
testLoadPackageDocs packageName =
    BackendTask.Custom.run "loadPackageDocs"
        (Encode.string packageName)
        (Decode.list Docs.decoder)
        |> BackendTask.allowFatal


encodeApiDiff : ApiDiff -> Encode.Value
encodeApiDiff diff =
    Encode.object
        [ ( "magnitude", Encode.string diff.magnitude )
        , ( "addedModules", Encode.list Encode.string diff.addedModules )
        , ( "removedModules", Encode.list Encode.string diff.removedModules )
        , ( "changedModules"
          , Encode.list
                (\mc ->
                    Encode.object
                        [ ( "name", Encode.string mc.name )
                        , ( "added", Encode.list Encode.string mc.added )
                        , ( "changed", Encode.list Encode.string mc.changed )
                        , ( "removed", Encode.list Encode.string mc.removed )
                        ]
                )
                diff.changedModules
          )
        , ( "commentDiffs", Encode.object [] )
        , ( "oldTypes", Encode.object [] )
        ]



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


multiValueModule : Docs.Module
multiValueModule =
    { name = "Example"
    , comment = "Example module.\n\n@docs foo, bar"
    , unions = []
    , aliases = []
    , values =
        [ { name = "foo"
          , comment = "First function."
          , tipe = Type.Type "String" []
          }
        , { name = "bar"
          , comment = "Second function."
          , tipe = Type.Type "Int" []
          }
        ]
    , binops = []
    }


sectionedModule : Docs.Module
sectionedModule =
    { name = "Example"
    , comment = "Example module.\n\n## Basics\n\n@docs foo\n\n## Advanced\n\n@docs bar"
    , unions = []
    , aliases = []
    , values =
        [ { name = "foo"
          , comment = "A basic function."
          , tipe = Type.Type "String" []
          }
        , { name = "bar"
          , comment = "An advanced function."
          , tipe = Type.Type "Int" []
          }
        ]
    , binops = []
    }


radiusModule : Docs.Module
radiusModule =
    { name = "W.Theme.Radius"
    , comment = "Radius tokens.\n\n@docs RadiusScale, Radius\n\n## Radius\n\n@docs none, xs, sm, md, lg, xl, xl2, xl3, full, custom, toCSS"
    , unions = []
    , aliases =
        [ { name = "RadiusScale"
          , comment = "The default radius scale."
          , args = []
          , tipe =
                Type.Record
                    [ ( "xs", Type.Type "Float" [] )
                    , ( "sm", Type.Type "Float" [] )
                    , ( "md", Type.Type "Float" [] )
                    , ( "lg", Type.Type "Float" [] )
                    , ( "xl", Type.Type "Float" [] )
                    , ( "xl2", Type.Type "Float" [] )
                    , ( "xl3", Type.Type "Float" [] )
                    ]
                    Nothing
          }
        , { name = "Radius"
          , comment = "An opaque radius type."
          , args = []
          , tipe = Type.Type "Float" []
          }
        ]
    , values =
        [ { name = "none", comment = "No radius.", tipe = Type.Type "Radius" [] }
        , { name = "xs", comment = "Extra small radius.", tipe = Type.Type "Radius" [] }
        , { name = "sm", comment = "Small radius.", tipe = Type.Type "Radius" [] }
        , { name = "md", comment = "Medium radius.", tipe = Type.Type "Radius" [] }
        , { name = "lg", comment = "Large radius.", tipe = Type.Type "Radius" [] }
        , { name = "xl", comment = "Extra large radius.", tipe = Type.Type "Radius" [] }
        , { name = "xl2", comment = "Extra large 2 radius.", tipe = Type.Type "Radius" [] }
        , { name = "xl3", comment = "Extra large 3 radius.", tipe = Type.Type "Radius" [] }
        , { name = "full", comment = "Full radius.", tipe = Type.Type "Radius" [] }
        , { name = "custom", comment = "Custom radius.", tipe = Type.Lambda (Type.Type "Float" []) (Type.Type "Radius" []) }
        , { name = "toCSS", comment = "Convert to CSS.", tipe = Type.Lambda (Type.Type "Radius" []) (Type.Type "String" []) }
        ]
    , binops = []
    }


cliOptionModule : Docs.Module
cliOptionModule =
    { name = "Cli.Option.Typed"
    , comment = "Typed CLI options.\n\n## When to Use This\n\nUse when you need typed options.\n\n## Example\n\n    type alias Options =\n        { name : String }\n\n@docs Option, CliDecoder\n\n## Decoders\n\n@docs string, int"
    , unions =
        [ { name = "CliDecoder"
          , comment = "A decoder for CLI values."
          , args = [ "value" ]
          , tags = [ ( "StringDecoder", [] ), ( "IntDecoder", [] ) ]
          }
        ]
    , aliases =
        [ { name = "Option"
          , comment = "The type for an option."
          , args = [ "from", "to" ]
          , tipe = Type.Type "String" []
          }
        ]
    , values =
        [ { name = "string"
          , comment = "A string option."
          , tipe = Type.Type "CliDecoder" [ Type.Type "String" [] ]
          }
        , { name = "int"
          , comment = "An int option."
          , tipe = Type.Type "CliDecoder" [ Type.Type "Int" [] ]
          }
        ]
    , binops = []
    }


linkModule : Docs.Module
linkModule =
    { name = "MyModule"
    , comment = "A module with links.\n\nSee [`Http`](Http) for HTTP requests.\n\n@docs myFunction"
    , unions = []
    , aliases = []
    , values =
        [ { name = "myFunction"
          , comment = "Does something."
          , tipe = Type.Type "String" []
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



sampleDiffWithReadme : ApiDiff
sampleDiffWithReadme =
    { magnitude = "MINOR"
    , addedModules = []
    , removedModules = []
    , changedModules = []
    , commentDiffs = Dict.empty
    , readmeDiff = Just "- Old intro line\n+ New intro line\n  Unchanged line"
    , oldTypes = Dict.empty
    }



-- STEPPER PIPELINES


run : Script.Script
run =
    Tui.Test.Stepper.runNamed
        [ ( "browseMode", TuiTest.toSnapshots browseMode )
        , ( "diffMode", TuiTest.toSnapshots diffMode )
        , ( "versionsTab", TuiTest.toSnapshots versionsTab )
        , ( "loadDiffFromVersions", TuiTest.toSnapshots loadDiffFromVersions )
        ]


browseMode : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
browseMode =
    startBrowse treeModules
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey '/'
        |> TuiTest.pressKey 'h'
        |> TuiTest.pressKey 't'
        |> TuiTest.pressKeyWith { key = Tui.Escape, modifiers = [] }


diffMode : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
diffMode =
    startWithDiff sampleDiff treeModules
        |> TuiTest.pressKey 'd'
        |> TuiTest.pressKey 'd'
        |> TuiTest.pressKey '1'
        |> TuiTest.pressKey 'd'


versionsTab : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
versionsTab =
    startWithVersions [ "12.1.0", "12.0.0", "11.0.0" ] treeModules
        |> TuiTest.pressKey 'v'
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey '1'


loadDiffFromVersions : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
loadDiffFromVersions =
    startWithVersions [ "12.1.0" ] treeModules
        |> TuiTest.pressKey 'v'
        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
        |> TuiTest.resolveEffect
            (BackendTaskTest.simulateCustom "loadDiff"
                (encodeApiDiff sampleDiff)
            )
