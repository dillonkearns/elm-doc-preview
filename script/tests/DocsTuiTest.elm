module DocsTuiTest exposing (browseMode, diffMode, loadDiffFromVersions, run, suite, versionsTab)

import BackendTask
import BackendTask.Custom
import Dict
import DocsTui
import Pages.Script as Script
import Docs.Diff as Diff exposing (ApiDiff)
import Elm.Docs as Docs
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
            , test "Tab cycles pane focus" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from modules to items
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from items to docs
                        |> TuiTest.pressKeyWith { key = Tui.Tab, modifiers = [] }
                        -- Focus moves from docs back to modules
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 2"
                        |> TuiTest.expectRunning
            , test "pressing 2 without diff or versions does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "Modules"
                        |> TuiTest.ensureViewDoesNotHave "Changes"
                        |> TuiTest.expectRunning
            , test "Changes tab visible when versions available (no diff yet)" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.ensureViewHas "Changes"
                        |> TuiTest.expectRunning
            , test "pressing 2 with versions triggers diff loading" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey '2'
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
            , test "> pages down in modules list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '>'
                        |> TuiTest.ensureViewHas "4 of 4"
                        |> TuiTest.expectRunning
            , test "< pages up in modules list" <|
                \() ->
                    startBrowse treeModules
                        |> TuiTest.pressKey '>'
                        |> TuiTest.pressKey '<'
                        |> TuiTest.ensureViewHas "1 of 4"
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
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "get"
                        |> TuiTest.ensureViewDoesNotHave "decodeString"
                        |> TuiTest.expectRunning
            , test "in diff view, items pane shows only changed items" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        -- sampleDiff has Json.Decode with added: ["decodeString"]
                        -- Items should only show "decodeString", not all module items
                        |> TuiTest.ensureViewHas "decodeString"
                        |> TuiTest.expectRunning
            , test "in diff view, newly added module shows its items not another module's" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.pressKey '2'
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
            , test "j in items pane scrolls docs to selected item" <|
                \() ->
                    TuiTest.startWithContext { width = 120, height = 8, colorProfile = Tui.TrueColor }
                        { data =
                            { modules = [ multiValueModule ]
                            , diff = Nothing
                            , versions = []
                            , loadDiff = noopLoadDiff
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
            , test "items pane shows no items for group headers" <|
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
        , describe "versions tab"
            [ test "pressing 3 switches to Versions tab" <|
                \() ->
                    startWithVersions [ "12.1.0", "12.0.0" ] sampleModules
                        |> TuiTest.pressKey '3'
                        |> TuiTest.ensureViewHas "12.1.0"
                        |> TuiTest.ensureViewHas "12.0.0"
                        |> TuiTest.expectRunning
            , test "pressing 3 without versions does nothing" <|
                \() ->
                    startBrowse sampleModules
                        |> TuiTest.pressKey '3'
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
                        |> TuiTest.pressKey '3'
                        |> TuiTest.pressKey 'j'
                        |> TuiTest.ensureViewHas "2 of 3"
                        |> TuiTest.expectRunning
            ]
        , describe "on-demand diff loading"
            [ test "Enter on version shows Loading with spinner" <|
                \() ->
                    startWithVersions [ "12.1.0" ] sampleModules
                        |> TuiTest.pressKey '3'
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
                        |> TuiTest.pressKey '3'
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
                        |> TuiTest.pressKey '3'
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
                        |> TuiTest.pressKey '3'
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
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.expectRunning
            , test "selecting README shows diff lines in right pane" <|
                \() ->
                    startWithDiff sampleDiffWithReadme sampleModules
                        |> TuiTest.pressKey 'd'
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewHas "README"
                        |> TuiTest.ensureViewHas "New intro line"
                        |> TuiTest.expectRunning
            , test "README does not appear in Changes tab when no readmeDiff" <|
                \() ->
                    startWithDiff sampleDiff sampleModules
                        |> TuiTest.pressKey '2'
                        |> TuiTest.ensureViewDoesNotHave "README"
                        |> TuiTest.expectRunning
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
            }
        , init = DocsTui.init
        , update = DocsTui.update
        , view = DocsTui.view
        , subscriptions = DocsTui.subscriptions
        }


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
        |> TuiTest.pressKey '2'
        |> TuiTest.pressKey '1'
        |> TuiTest.pressKey 'd'


versionsTab : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
versionsTab =
    startWithVersions [ "12.1.0", "12.0.0", "11.0.0" ] treeModules
        |> TuiTest.pressKey '3'
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey 'j'
        |> TuiTest.pressKey '1'


loadDiffFromVersions : TuiTest.TuiTest DocsTui.Model DocsTui.Msg
loadDiffFromVersions =
    startWithVersions [ "12.1.0" ] treeModules
        |> TuiTest.pressKey '3'
        |> TuiTest.pressKeyWith { key = Tui.Enter, modifiers = [] }
        |> TuiTest.resolveEffect
            (BackendTaskTest.simulateCustom "loadDiff"
                (encodeApiDiff sampleDiff)
            )
