module DocsTui exposing (Model, Msg, init, subscriptions, update, view)

{-| Unified TUI for browsing Elm documentation and viewing API diffs.

Supports two modes:

  - **Browse mode**: No diff data, shows all modules with full docs in the right pane.
  - **Diff mode**: With diff data, `d` toggles between docs view and diff view.

Left pane: list of modules (with diff badges when in diff view).
Right pane: full docs for the selected module, or diff details when toggled.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import Dict
import Docs.Diff as Diff exposing (ApiDiff, ModuleChanges)
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import ModuleTree exposing (TreeEntry(..))
import SyntaxHighlight
import Tui
import Tui.Effect as Effect
import Tui.Input as Input
import Tui.Keybinding as Keybinding
import Tui.Layout as Layout
import Tui.Modal as Modal
import Tui.FuzzyMatch as FuzzyMatch
import Tui.Spinner
import Tui.Sub


type alias Model =
    { layout : Layout.State
    , modules : List Docs.Module
    , diff : Maybe ApiDiff
    , rightView : RightView
    , showHelp : Bool
    , moduleTree : ModuleTree.ModuleTree
    , activeLeftTab : LeftTab
    , filterInput : Maybe Input.State
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    , loadingDiff : Bool
    , diffVersion : Maybe String
    , spinnerTick : Int
    }


type RightView
    = DocsView
    | DiffView


type LeftTab
    = ModulesTab
    | ChangesTab
    | VersionsTab


type Msg
    = KeyPressed Tui.KeyEvent
    | MouseEvent Tui.MouseEvent
    | SelectEntry Int
    | SelectItem Int
    | GotContext { width : Int, height : Int }
    | GotDiff (Maybe ApiDiff)
    | SpinnerTick


init :
    { modules : List Docs.Module
    , diff : Maybe ApiDiff
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    }
    -> ( Model, Effect.Effect Msg )
init { modules, diff, versions, loadDiff } =
    ( { layout =
            Layout.init
                |> Layout.focusPane "modules"
      , modules = modules
      , diff = diff
      , rightView = DocsView
      , showHelp = False
      , moduleTree = ModuleTree.build (List.map .name modules)
      , activeLeftTab = ModulesTab
      , filterInput = Nothing
      , versions = versions
      , loadDiff = loadDiff
      , loadingDiff = False
      , diffVersion = Nothing
      , spinnerTick = 0
      }
    , Effect.none
    )


visibleEntries : Model -> List TreeEntry
visibleEntries model =
    let
        baseEntries =
            case model.activeLeftTab of
                ModulesTab ->
                    ModuleTree.visibleEntries model.moduleTree

                ChangesTab ->
                    case model.diff of
                        Just diff ->
                            changedModuleEntries diff

                        Nothing ->
                            ModuleTree.visibleEntries model.moduleTree

                VersionsTab ->
                    model.versions
                        |> List.map (\v -> Leaf { name = v, depth = 0 })
    in
    case model.filterInput of
        Just input ->
            let
                query =
                    String.toLower (Input.text input)
            in
            if String.isEmpty query then
                baseEntries

            else
                baseEntries
                    |> List.filter
                        (\entry ->
                            FuzzyMatch.match query (ModuleTree.entryName entry)
                        )

        Nothing ->
            baseEntries


changedModuleEntries : ApiDiff -> List TreeEntry
changedModuleEntries diff =
    let
        readmeEntry =
            case diff.readmeDiff of
                Just _ ->
                    [ Leaf { name = "README", depth = 0 } ]

                Nothing ->
                    []

        allChangedNames =
            diff.addedModules
                ++ List.map .name diff.changedModules
                ++ diff.removedModules
                ++ Dict.keys diff.commentDiffs
                |> List.foldl
                    (\name acc ->
                        if List.member name acc then
                            acc

                        else
                            acc ++ [ name ]
                    )
                    []

        moduleEntries =
            allChangedNames
                |> List.map (\name -> Leaf { name = name, depth = 0 })
    in
    readmeEntry ++ moduleEntries


selectedEntry : Model -> Maybe TreeEntry
selectedEntry model =
    let
        idx =
            Layout.selectedIndex "modules" model.layout
    in
    visibleEntries model
        |> List.drop idx
        |> List.head


selectedModule : Model -> Maybe Docs.Module
selectedModule model =
    case selectedEntry model of
        Just (Leaf { name }) ->
            findModule name model.modules

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Effect.Effect Msg )
update msg model =
    case msg of
        KeyPressed event ->
            case model.filterInput of
                Just input ->
                    -- In filter mode: Escape cancels, everything else goes to input
                    if event.key == Tui.Escape then
                        ( { model | filterInput = Nothing }, Effect.none )

                    else
                        ( { model | filterInput = Just (Input.update event input) }
                        , Effect.none
                        )

                Nothing ->
                    case Keybinding.dispatch (allBindings model) event of
                        Just action ->
                            handleAction action model

                        Nothing ->
                            ( model, Effect.none )

        MouseEvent mouseEvent ->
            let
                ctx =
                    Layout.contextOf model.layout

                ( newLayout, maybeMsg ) =
                    Layout.handleMouse mouseEvent ctx (viewLayout ctx model) model.layout
            in
            case maybeMsg of
                Just subMsg ->
                    update subMsg { model | layout = newLayout }

                Nothing ->
                    ( { model | layout = newLayout }, Effect.none )

        SelectEntry _ ->
            ( { model
                | layout =
                    model.layout
                        |> Layout.resetScroll "docs"
              }
            , Effect.none
            )

        SelectItem idx ->
            let
                items =
                    itemsForCurrentView model

                maybeItemName =
                    items |> List.drop idx |> List.head

                docsContent =
                    currentDocsContent model

                scrollTarget =
                    case maybeItemName of
                        Just itemName ->
                            findLineForItem itemName docsContent

                        Nothing ->
                            0

                newLayout =
                    model.layout
                        |> Layout.resetScroll "docs"
                        |> Layout.scrollDown "docs" scrollTarget
            in
            ( { model | layout = newLayout }
            , Effect.none
            )

        GotContext ctx ->
            ( { model
                | layout =
                    Layout.withContext ctx model.layout
              }
            , Effect.none
            )

        GotDiff maybeDiff ->
            ( { model
                | diff = maybeDiff
                , loadingDiff = False
                , rightView =
                    case maybeDiff of
                        Just _ ->
                            DiffView

                        Nothing ->
                            model.rightView
              }
            , Effect.none
            )

        SpinnerTick ->
            ( { model | spinnerTick = model.spinnerTick + 1 }, Effect.none )


type Action
    = Quit
    | NavigateDown
    | NavigateUp
    | FocusModules
    | FocusItems
    | FocusDocs
    | FocusLeft
    | FocusRight
    | ScrollDocsDown
    | ScrollDocsUp
    | ToggleHelp
    | CloseHelp
    | ToggleDiffView
    | ToggleTreeNode
    | SwitchToModulesTab
    | SwitchToChangesTab
    | SwitchToVersionsTab
    | CycleLeftTab
    | ActivateFilter
    | CancelFilter
    | LoadDiffForVersion
    | PageDown
    | PageUp
    | ScrollDocsPageDown
    | ScrollDocsPageUp
    | ToggleMaximize


handleAction : Action -> Model -> ( Model, Effect.Effect Msg )
handleAction action model =
    case action of
        Quit ->
            if Layout.isMaximized "docs" model.layout then
                ( { model | layout = Layout.toggleMaximize "docs" model.layout }
                , Effect.none
                )

            else
                ( model, Effect.exit )

        NavigateDown ->
            let
                ctx =
                    Layout.contextOf model.layout

                paneId =
                    Layout.focusedPane model.layout |> Maybe.withDefault "modules"

                ( newLayout, maybeMsg ) =
                    Layout.navigateDown paneId (viewLayout ctx model) model.layout
            in
            case maybeMsg of
                Just subMsg ->
                    update subMsg { model | layout = newLayout }

                Nothing ->
                    ( { model | layout = newLayout }
                    , Effect.none
                    )

        NavigateUp ->
            let
                ctx =
                    Layout.contextOf model.layout

                paneId =
                    Layout.focusedPane model.layout |> Maybe.withDefault "modules"

                ( newLayout, maybeMsg ) =
                    Layout.navigateUp paneId (viewLayout ctx model) model.layout
            in
            case maybeMsg of
                Just subMsg ->
                    update subMsg { model | layout = newLayout }

                Nothing ->
                    ( { model | layout = newLayout }
                    , Effect.none
                    )

        FocusModules ->
            ( { model | layout = Layout.focusPane "modules" model.layout }
            , Effect.none
            )

        FocusItems ->
            ( { model | layout = Layout.focusPane "items" model.layout }
            , Effect.none
            )

        FocusDocs ->
            ( { model | layout = Layout.focusPane "docs" model.layout }
            , Effect.none
            )

        FocusLeft ->
            let
                newPane =
                    case Layout.focusedPane model.layout of
                        Just "docs" ->
                            "items"

                        Just "items" ->
                            "modules"

                        _ ->
                            "modules"
            in
            ( { model | layout = Layout.focusPane newPane model.layout }
            , Effect.none
            )

        FocusRight ->
            let
                newPane =
                    case Layout.focusedPane model.layout of
                        Just "modules" ->
                            "items"

                        Just "items" ->
                            "docs"

                        _ ->
                            "docs"
            in
            ( { model | layout = Layout.focusPane newPane model.layout }
            , Effect.none
            )

        ScrollDocsDown ->
            ( { model | layout = Layout.scrollDown "docs" 3 model.layout }
            , Effect.none
            )

        ScrollDocsUp ->
            ( { model | layout = Layout.scrollUp "docs" 3 model.layout }
            , Effect.none
            )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Effect.none )

        CloseHelp ->
            ( { model | showHelp = False }, Effect.none )

        ToggleDiffView ->
            if model.loadingDiff then
                ( model, Effect.none )

            else
                case model.diff of
                    Just _ ->
                        let
                            newView =
                                case model.rightView of
                                    DocsView ->
                                        DiffView

                                    DiffView ->
                                        DocsView
                        in
                        ( { model | rightView = newView }, Effect.none )

                    Nothing ->
                        case model.versions of
                            firstVersion :: _ ->
                                ( { model | loadingDiff = True, diffVersion = Just firstVersion }
                                , Effect.perform GotDiff (model.loadDiff firstVersion)
                                )

                            [] ->
                                ( model, Effect.none )

        ToggleTreeNode ->
            case selectedEntry model of
                Just (Group { prefix }) ->
                    ( { model | moduleTree = ModuleTree.toggle prefix model.moduleTree }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        SwitchToModulesTab ->
            ( { model | activeLeftTab = ModulesTab, rightView = DocsView }, Effect.none )

        SwitchToChangesTab ->
            case model.diff of
                Just _ ->
                    ( { model | activeLeftTab = ChangesTab, rightView = DiffView }, Effect.none )

                Nothing ->
                    case model.versions of
                        firstVersion :: _ ->
                            if model.loadingDiff then
                                ( { model | activeLeftTab = ChangesTab, rightView = DiffView }, Effect.none )

                            else
                                ( { model
                                    | activeLeftTab = ChangesTab
                                    , rightView = DiffView
                                    , loadingDiff = True
                                    , diffVersion = Just firstVersion
                                  }
                                , Effect.perform GotDiff (model.loadDiff firstVersion)
                                )

                        [] ->
                            ( model, Effect.none )

        CycleLeftTab ->
            let
                hasChangesTab =
                    model.diff /= Nothing || not (List.isEmpty model.versions)

                hasVersionsTab =
                    not (List.isEmpty model.versions)

                nextTab =
                    case model.activeLeftTab of
                        ModulesTab ->
                            if hasChangesTab then
                                ChangesTab

                            else if hasVersionsTab then
                                VersionsTab

                            else
                                ModulesTab

                        ChangesTab ->
                            if hasVersionsTab then
                                VersionsTab

                            else
                                ModulesTab

                        VersionsTab ->
                            ModulesTab
            in
            let
                newRightView =
                    case nextTab of
                        ChangesTab ->
                            DiffView

                        _ ->
                            DocsView
            in
            if nextTab == ChangesTab && model.diff == Nothing then
                -- Need to load diff first
                case model.versions of
                    firstVersion :: _ ->
                        if model.loadingDiff then
                            ( { model | activeLeftTab = nextTab, rightView = newRightView }, Effect.none )

                        else
                            ( { model
                                | activeLeftTab = nextTab
                                , rightView = newRightView
                                , loadingDiff = True
                                , diffVersion = Just firstVersion
                              }
                            , Effect.perform GotDiff (model.loadDiff firstVersion)
                            )

                    [] ->
                        ( { model | activeLeftTab = nextTab, rightView = newRightView }, Effect.none )

            else
                ( { model | activeLeftTab = nextTab, rightView = newRightView }, Effect.none )

        ActivateFilter ->
            ( { model | filterInput = Just (Input.init "") }, Effect.none )

        CancelFilter ->
            ( { model | filterInput = Nothing }, Effect.none )

        SwitchToVersionsTab ->
            if List.isEmpty model.versions then
                ( model, Effect.none )

            else
                ( { model | activeLeftTab = VersionsTab }, Effect.none )

        LoadDiffForVersion ->
            if model.loadingDiff then
                ( model, Effect.none )

            else
                case selectedEntry model of
                    Just (Leaf { name }) ->
                        ( { model | loadingDiff = True, diffVersion = Just name }
                        , Effect.perform GotDiff (model.loadDiff name)
                        )

                    _ ->
                        ( model, Effect.none )

        PageDown ->
            let
                ctx =
                    Layout.contextOf model.layout

                paneId =
                    Layout.focusedPane model.layout |> Maybe.withDefault "modules"

                pageSize =
                    max 1 ((ctx.height - 4) // 2)

                step i ( m, _ ) =
                    if i <= 0 then
                        ( m, Effect.none )

                    else
                        let
                            ( newLayout, _ ) =
                                Layout.navigateDown paneId (viewLayout ctx m) m.layout
                        in
                        step (i - 1) ( { m | layout = newLayout }, Effect.none )
            in
            step pageSize ( model, Effect.none )

        PageUp ->
            let
                ctx =
                    Layout.contextOf model.layout

                paneId =
                    Layout.focusedPane model.layout |> Maybe.withDefault "modules"

                pageSize =
                    max 1 ((ctx.height - 4) // 2)

                step i ( m, _ ) =
                    if i <= 0 then
                        ( m, Effect.none )

                    else
                        let
                            ( newLayout, _ ) =
                                Layout.navigateUp paneId (viewLayout ctx m) m.layout
                        in
                        step (i - 1) ( { m | layout = newLayout }, Effect.none )
            in
            step pageSize ( model, Effect.none )

        ScrollDocsPageDown ->
            let
                ctx =
                    Layout.contextOf model.layout

                pageSize =
                    max 1 (ctx.height - 4)
            in
            ( { model | layout = Layout.scrollDown "docs" pageSize model.layout }
            , Effect.none
            )

        ScrollDocsPageUp ->
            let
                ctx =
                    Layout.contextOf model.layout

                pageSize =
                    max 1 (ctx.height - 4)
            in
            ( { model | layout = Layout.scrollUp "docs" pageSize model.layout }
            , Effect.none
            )

        ToggleMaximize ->
            ( { model | layout = Layout.toggleMaximize "docs" model.layout }
            , Effect.none
            )



-- KEYBINDINGS


globalBindings : Model -> Keybinding.Group Action
globalBindings model =
    let
        hasDiff =
            model.diff /= Nothing

        hasVersions =
            not (List.isEmpty model.versions)

        diffToggleBinding =
            if hasDiff || hasVersions then
                [ Keybinding.binding (Tui.Character 'd') "Toggle diff view" ToggleDiffView ]

            else
                []

        changesTabBinding =
            if hasDiff || hasVersions then
                [ Keybinding.binding (Tui.Character '2') "Changes tab" SwitchToChangesTab ]

            else
                []

        versionsTabBinding =
            if hasVersions then
                [ Keybinding.binding (Tui.Character '3') "Versions tab" SwitchToVersionsTab ]

            else
                []

        cycleBinding =
            if hasDiff || hasVersions then
                [ Keybinding.binding Tui.Tab "Cycle tabs" CycleLeftTab ]

            else
                []
    in
    Keybinding.group "Global"
        ([ Keybinding.binding (Tui.Character 'q') "Quit" Quit
         , Keybinding.binding Tui.Escape "Quit" Quit
         , Keybinding.binding (Tui.Character '?') "Toggle help" ToggleHelp
         , Keybinding.binding (Tui.Character 'h') "Focus left" FocusLeft
             |> Keybinding.withAlternate (Tui.Arrow Tui.Left)
         , Keybinding.binding (Tui.Character 'l') "Focus right" FocusRight
             |> Keybinding.withAlternate (Tui.Arrow Tui.Right)
         , Keybinding.binding (Tui.Character '1') "Modules tab" SwitchToModulesTab
         ]
            ++ diffToggleBinding
            ++ changesTabBinding
            ++ versionsTabBinding
            ++ cycleBinding
        )


helpBindings : Keybinding.Group Action
helpBindings =
    Keybinding.group "Help"
        [ Keybinding.binding (Tui.Character '?') "Close help" ToggleHelp
        , Keybinding.binding Tui.Escape "Close help" CloseHelp
        , Keybinding.binding (Tui.Character 'q') "Quit" Quit
        ]


modulesBindings : Model -> Keybinding.Group Action
modulesBindings model =
    let
        enterAction =
            case model.activeLeftTab of
                VersionsTab ->
                    LoadDiffForVersion

                _ ->
                    ToggleTreeNode
    in
    Keybinding.group "Modules"
        [ Keybinding.binding (Tui.Character 'j') "Next module" NavigateDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Previous module" NavigateUp
            |> Keybinding.withAlternate (Tui.Arrow Tui.Up)
        , Keybinding.binding (Tui.Character '>') "Page down" PageDown
            |> Keybinding.withAlternate Tui.PageDown
        , Keybinding.binding (Tui.Character '<') "Page up" PageUp
            |> Keybinding.withAlternate Tui.PageUp
        , Keybinding.binding Tui.Enter "Select" enterAction
        , Keybinding.binding (Tui.Character '/') "Filter" ActivateFilter
        ]


itemsBindings : Keybinding.Group Action
itemsBindings =
    Keybinding.group "Items"
        [ Keybinding.binding (Tui.Character 'j') "Next item" NavigateDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Previous item" NavigateUp
            |> Keybinding.withAlternate (Tui.Arrow Tui.Up)
        , Keybinding.binding (Tui.Character '>') "Page down" PageDown
            |> Keybinding.withAlternate Tui.PageDown
        , Keybinding.binding (Tui.Character '<') "Page up" PageUp
            |> Keybinding.withAlternate Tui.PageUp
        ]


docsBindings : Keybinding.Group Action
docsBindings =
    Keybinding.group "Docs"
        [ Keybinding.binding (Tui.Character 'j') "Scroll down" ScrollDocsDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Scroll up" ScrollDocsUp
            |> Keybinding.withAlternate (Tui.Arrow Tui.Up)
        , Keybinding.binding (Tui.Character ' ') "Page down" ScrollDocsPageDown
            |> Keybinding.withAlternate (Tui.Character '>')
        , Keybinding.binding Tui.PageDown "Page down" ScrollDocsPageDown
        , Keybinding.binding (Tui.Character '<') "Page up" ScrollDocsPageUp
            |> Keybinding.withAlternate Tui.PageUp
        , Keybinding.binding (Tui.Character 'o') "Maximize" ToggleMaximize
        ]


allBindings : Model -> List (Keybinding.Group Action)
allBindings model =
    if model.showHelp then
        [ helpBindings ]

    else
        let
            focusedBindings =
                case Layout.focusedPane model.layout of
                    Just "docs" ->
                        [ docsBindings ]

                    Just "items" ->
                        [ itemsBindings ]

                    _ ->
                        [ modulesBindings model ]
        in
        focusedBindings ++ [ globalBindings model ]



-- VIEW


view : Tui.Context -> Model -> Tui.Screen
view ctx model =
    let
        contentCtx =
            { width = ctx.width, height = ctx.height - 1 }

        layoutState =
            Layout.withContext contentCtx model.layout

        statusBar =
            renderStatusBar model ctx.width

        layoutRows =
            Layout.toRows layoutState (viewLayout contentCtx model)
    in
    if model.showHelp then
        let
            helpBody =
                Keybinding.helpRows "" [ modulesBindings model, docsBindings, globalBindings model ]
        in
        Modal.overlay
            { title = "Keybindings"
            , body = helpBody
            , footer = "? close  q quit"
            , width = min 50 (ctx.width - 4)
            }
            { termWidth = ctx.width, termHeight = ctx.height - 1 }
            layoutRows
            ++ [ statusBar ]
            |> Tui.lines

    else
        (layoutRows ++ [ statusBar ])
            |> Tui.lines


viewLayout : { a | width : Int, height : Int } -> Model -> Layout.Layout Msg
viewLayout ctx model =
    Layout.horizontal
        [ modulesPane ctx model
        , itemsPane model
        , rightPane ctx model
        ]


renderStatusBar : Model -> Int -> Tui.Screen
renderStatusBar model width =
    let
        hints =
            case model.filterInput of
                Just _ ->
                    [ "Esc cancel", "type to filter" ]

                Nothing ->
                    let
                        base =
                            [ "j/k navigate", "/filter", "? help", "q quit" ]

                        hasDiff =
                            model.diff /= Nothing

                        hasVersions =
                            not (List.isEmpty model.versions)

                        diffHint =
                            if hasDiff || hasVersions then
                                [ "d diff" ]

                            else
                                []

                        showChanges =
                            hasDiff || hasVersions

                        tabHint =
                            case ( showChanges, hasVersions ) of
                                ( True, True ) ->
                                    [ "1/2/3 tabs" ]

                                ( True, False ) ->
                                    [ "1/2 tabs" ]

                                ( False, True ) ->
                                    [ "1/3 tabs" ]

                                ( False, False ) ->
                                    []
                    in
                    base ++ diffHint ++ tabHint

        hintText =
            String.join "  " hints

        padding =
            max 0 (width - String.length hintText)

        barStyle =
            { fg = Just Ansi.Color.black
            , bg = Just Ansi.Color.white
            , attributes = []
            }
    in
    Tui.styled barStyle (hintText ++ String.repeat padding " ")


modulesPaneWidth : Int -> Layout.Width
modulesPaneWidth termWidth =
    Layout.px (min 28 (termWidth // 4))


leftPaneTitle : Model -> String
leftPaneTitle model =
    let
        hasDiff =
            model.diff /= Nothing

        hasVersions =
            not (List.isEmpty model.versions)

        showChanges =
            hasDiff || hasVersions
    in
    case ( showChanges, hasVersions ) of
        ( True, True ) ->
            "[1]Modules [2]Changes [3]Versions"

        ( True, False ) ->
            "[1]Modules [2]Changes"

        ( False, True ) ->
            "[1]Modules [3]Versions"

        ( False, False ) ->
            "Modules"


leftPaneTitleScreen : Model -> Maybe Tui.Screen
leftPaneTitleScreen model =
    let
        hasDiff =
            model.diff /= Nothing

        hasVersions =
            not (List.isEmpty model.versions)

        tab : String -> String -> LeftTab -> Tui.Screen
        tab number label tabType =
            if model.activeLeftTab == tabType then
                Tui.concat
                    [ Tui.text number |> Tui.bold |> Tui.fg Ansi.Color.cyan
                    , Tui.text label |> Tui.bold
                    ]

            else
                Tui.text (number ++ label) |> Tui.dim

        separator =
            Tui.text " "
    in
    let
        showChanges =
            hasDiff || hasVersions
    in
    case ( showChanges, hasVersions ) of
        ( True, True ) ->
            Just
                (Tui.concat
                    [ tab "[1]" "Modules" ModulesTab
                    , separator
                    , tab "[2]" "Changes" ChangesTab
                    , separator
                    , tab "[3]" "Versions" VersionsTab
                    ]
                )

        ( True, False ) ->
            Just
                (Tui.concat
                    [ tab "[1]" "Modules" ModulesTab
                    , separator
                    , tab "[2]" "Changes" ChangesTab
                    ]
                )

        ( False, True ) ->
            Just
                (Tui.concat
                    [ tab "[1]" "Modules" ModulesTab
                    , separator
                    , tab "[3]" "Versions" VersionsTab
                    ]
                )

        ( False, False ) ->
            Nothing


modulesPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
modulesPane ctx model =
    let
        entries =
            visibleEntries model

        entryCount =
            List.length entries

        showBadges =
            (model.rightView == DiffView || model.activeLeftTab == ChangesTab)
                && model.diff /= Nothing
    in
    let
        pane =
            Layout.pane "modules"
                { title = leftPaneTitle model
                , width = modulesPaneWidth ctx.width
                }
                (Layout.selectableList
                    { onSelect = SelectEntry
                    , selected = \entry -> renderTreeEntrySelected showBadges model entry
                    , default = \entry -> renderTreeEntryDefault showBadges model entry
                    }
                    entries
                )

        styledPane =
            case leftPaneTitleScreen model of
                Just titleScreen ->
                    pane |> Layout.withTitleScreen titleScreen

                Nothing ->
                    pane
    in
    styledPane
        |> Layout.withPrefix ("[" ++ String.fromInt entryCount ++ "] ")
        |> Layout.withFooter
            (case model.filterInput of
                Just input ->
                    "/" ++ Input.text input

                Nothing ->
                    String.fromInt (Layout.selectedIndex "modules" model.layout + 1)
                        ++ " of "
                        ++ String.fromInt entryCount
            )


itemsForCurrentView : Model -> List String
itemsForCurrentView model =
    case model.rightView of
        DiffView ->
            case ( model.diff, selectedEntry model ) of
                ( Just diff, Just (Leaf { name }) ) ->
                    if List.member name diff.addedModules then
                        -- New module: show all items from the module
                        case findModule name model.modules of
                            Just mod ->
                                moduleItemNames mod

                            Nothing ->
                                []

                    else
                        diffItemNames diff name

                _ ->
                    []

        DocsView ->
            case selectedModule model of
                Just mod ->
                    moduleItemNames mod

                Nothing ->
                    []


diffItemNames : ApiDiff -> String -> List String
diffItemNames diff moduleName =
    case Diff.findModuleChanges moduleName diff.changedModules of
        Just changes ->
            changes.added ++ changes.changed ++ changes.removed

        Nothing ->
            if List.member moduleName diff.addedModules then
                -- New module: show all items
                []

            else
                []


itemsPane : Model -> Layout.Pane Msg
itemsPane model =
    let
        items =
            itemsForCurrentView model

        itemCount =
            List.length items
    in
    Layout.pane "items"
        { title = "Items"
        , width = Layout.px 20
        }
        (Layout.selectableList
            { onSelect = SelectItem
            , selected =
                \name ->
                    Tui.styled
                        { fg = Just Ansi.Color.white
                        , bg = Just Ansi.Color.blue
                        , attributes = [ Tui.Bold ]
                        }
                        (name ++ " ")
            , default = \name -> Tui.text name
            }
            items
        )
        |> Layout.withFooter
            (if itemCount > 0 then
                String.fromInt (Layout.selectedIndex "items" model.layout + 1)
                    ++ " of "
                    ++ String.fromInt itemCount

             else
                ""
            )


currentDocsContent : Model -> List Tui.Screen
currentDocsContent model =
    case model.rightView of
        DocsView ->
            case selectedModule model of
                Just mod ->
                    renderModuleDocs mod

                Nothing ->
                    []

        DiffView ->
            case ( model.diff, selectedEntry model ) of
                ( Just diff, Just (Leaf { name }) ) ->
                    if name == "README" then
                        renderReadmeDiff diff

                    else
                        [ magnitudeLine diff.magnitude, Tui.text "" ]
                            ++ renderModuleDiff diff model.modules name

                _ ->
                    []


findLineForItem : String -> List Tui.Screen -> Int
findLineForItem itemName lines =
    lines
        |> List.indexedMap Tuple.pair
        |> List.filter
            (\( _, line ) ->
                let
                    text =
                        Tui.toString line
                in
                String.contains itemName text
                    && (String.contains (itemName ++ " :") text
                            || String.contains (itemName ++ " =") text
                            || String.contains ("type " ++ itemName) text
                            || String.contains ("type alias " ++ itemName) text
                       )
            )
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


moduleItemNames : Docs.Module -> List String
moduleItemNames mod =
    let
        valueNames =
            List.map .name mod.values

        unionNames =
            List.map .name mod.unions

        aliasNames =
            List.map .name mod.aliases

        binopNames =
            List.map .name mod.binops
    in
    valueNames ++ unionNames ++ aliasNames ++ binopNames


renderTreeEntrySelected : Bool -> Model -> TreeEntry -> Tui.Screen
renderTreeEntrySelected showBadges model entry =
    let
        indent =
            case entry of
                Leaf { depth } ->
                    String.repeat (depth * 2) " "

                Group { depth } ->
                    String.repeat (depth * 2) " "

        prefix =
            case entry of
                Group { expanded } ->
                    if expanded then
                        "▾ "

                    else
                        "▸ "

                Leaf _ ->
                    ""

        name =
            ModuleTree.entryName entry

        badge =
            if showBadges then
                moduleBadge model name

            else
                Tui.empty
    in
    Tui.concat
        [ badge
        , Tui.styled
            { fg = Just Ansi.Color.white
            , bg = Just Ansi.Color.blue
            , attributes = [ Tui.Bold ]
            }
            (indent ++ prefix ++ name ++ " ")
        ]


renderTreeEntryDefault : Bool -> Model -> TreeEntry -> Tui.Screen
renderTreeEntryDefault showBadges model entry =
    let
        indent =
            case entry of
                Leaf { depth } ->
                    String.repeat (depth * 2) " "

                Group { depth } ->
                    String.repeat (depth * 2) " "

        prefix =
            case entry of
                Group { expanded } ->
                    if expanded then
                        "▾ "

                    else
                        "▸ "

                Leaf _ ->
                    ""

        name =
            ModuleTree.entryName entry

        badge =
            if showBadges then
                moduleBadge model name

            else
                Tui.empty
    in
    Tui.concat
        [ badge
        , Tui.text (indent ++ prefix ++ name)
        ]


moduleBadge : Model -> String -> Tui.Screen
moduleBadge model moduleName =
    case model.diff of
        Just diff ->
            case Diff.moduleStatus diff moduleName of
                Diff.ModuleNew ->
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ] } " + "

                Diff.ModuleChanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ] } " ~ "

                Diff.ModuleRemoved ->
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ] } " - "

                Diff.ModuleUnchanged ->
                    Tui.text "   "

        Nothing ->
            Tui.empty



-- RIGHT PANE


rightPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
rightPane _ model =
    if model.loadingDiff then
        Layout.pane "docs"
            { title = "Loading..."
            , width = Layout.fill
            }
            (Layout.content
                [ Tui.concat
                    [ Tui.text "Loading diff... " |> Tui.dim
                    , Tui.Spinner.view model.spinnerTick
                    ]
                ]
            )

    else
        case model.rightView of
            DocsView ->
                docsPane model

            DiffView ->
                diffPane model


docsPane : Model -> Layout.Pane Msg
docsPane model =
    let
        ( title, docsLines ) =
            case selectedEntry model of
                Just (Leaf { name }) ->
                    case findModule name model.modules of
                        Just mod ->
                            ( "Docs: " ++ name, renderModuleDocs mod )

                        Nothing ->
                            ( "Docs: " ++ name, [ Tui.text ("Module " ++ name ++ " not found") ] )

                Just (Group { prefix }) ->
                    ( "Docs: " ++ prefix, [ Tui.text ("Select a module under " ++ prefix) ] )

                Nothing ->
                    ( "Docs", [ Tui.text "No module selected" ] )
    in
    Layout.pane "docs"
        { title = title
        , width = Layout.fill
        }
        (Layout.content docsLines)
        |> withScrollPercentFooter model docsLines


diffPane : Model -> Layout.Pane Msg
diffPane model =
    let
        diffLines =
            case model.diff of
                Just diff ->
                    let
                        magnitudeHeader =
                            [ magnitudeLine diff.magnitude
                            , Tui.text ""
                            ]

                        entryDiffLines =
                            case selectedEntry model of
                                Just (Leaf { name }) ->
                                    if name == "README" then
                                        renderReadmeDiff diff

                                    else
                                        renderModuleDiff diff model.modules name

                                _ ->
                                    [ Tui.text "No module selected" ]
                    in
                    magnitudeHeader ++ entryDiffLines

                Nothing ->
                    [ Tui.text "No diff data" ]
    in
    let
        title =
            case model.diffVersion of
                Just version ->
                    "Diff vs " ++ version

                Nothing ->
                    "Diff"
    in
    Layout.pane "docs"
        { title = title
        , width = Layout.fill
        }
        (Layout.content diffLines)



withScrollPercentFooter : Model -> List Tui.Screen -> Layout.Pane Msg -> Layout.Pane Msg
withScrollPercentFooter model contentLines pane =
    let
        ctx =
            Layout.contextOf model.layout

        totalLines =
            List.length contentLines

        visibleHeight =
            ctx.height - 2

        scrollOffset =
            Layout.scrollPosition "docs" model.layout

        percent =
            if totalLines <= visibleHeight then
                ""

            else
                String.fromInt (min 100 (scrollOffset * 100 // max 1 (totalLines - visibleHeight))) ++ "%"
    in
    if String.isEmpty percent then
        pane

    else
        pane
            |> Layout.withFooterScreen
                (Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] } percent)



-- DOCS RENDERING


renderModuleDocs : Docs.Module -> List Tui.Screen
renderModuleDocs mod =
    let
        blocks =
            Docs.toBlocks mod

        separator =
            [ Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] }
                (String.repeat 40 "─")
            , Tui.text ""
            ]
    in
    blocks
        |> List.concatMap
            (\block ->
                case block of
                    Docs.MarkdownBlock _ ->
                        renderBlock block ++ [ Tui.text "" ]

                    _ ->
                        separator ++ renderBlock block ++ [ Tui.text "" ]
            )


renderBlock : Docs.Block -> List Tui.Screen
renderBlock block =
    case block of
        Docs.ValueBlock value ->
            renderValueTui value
                |> addGutter Ansi.Color.blue

        Docs.UnionBlock union ->
            renderUnionTui union
                |> addGutter Ansi.Color.green

        Docs.AliasBlock alias_ ->
            renderAliasTui alias_
                |> addGutter Ansi.Color.yellow

        Docs.BinopBlock binop ->
            renderBinopTui binop
                |> addGutter Ansi.Color.magenta

        Docs.MarkdownBlock markdown ->
            renderMarkdownToScreens (String.trim markdown)

        Docs.UnknownBlock name ->
            [ Tui.text ("  (unknown: " ++ name ++ ")") ]


addGutter : Ansi.Color.Color -> List Tui.Screen -> List Tui.Screen
addGutter color lines =
    let
        gutterChar =
            Tui.styled { fg = Just color, bg = Nothing, attributes = [] } "┃ "
    in
    List.map (\line -> Tui.concat [ gutterChar, line ]) lines


renderValueTui : Docs.Value -> List Tui.Screen
renderValueTui { name, comment, tipe } =
    let
        prefixWidth =
            String.length name + 3

        typeLines =
            Render.typeToLines prefixWidth tipe

        header =
            case typeLines of
                [ oneLine ] ->
                    [ Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } name
                        , Tui.text " : "
                        , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] } oneLine
                        ]
                    ]

                multipleLines ->
                    Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } name
                        , Tui.text " :"
                        ]
                        :: List.map
                            (\line ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                                    ("    " ++ line)
                            )
                            multipleLines

        commentLines =
            renderDocComment comment
    in
    header ++ commentLines


renderUnionTui : Docs.Union -> List Tui.Screen
renderUnionTui { name, comment, args, tags } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] }
                ("type " ++ name ++ typeVars)

        constructorLines =
            case tags of
                [] ->
                    []

                first :: rest ->
                    Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                        ("    = " ++ renderTag first)
                        :: List.map
                            (\t ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                                    ("    | " ++ renderTag t)
                            )
                            rest

        commentLines =
            renderDocComment comment
    in
    header :: constructorLines ++ commentLines


renderTag : ( String, List Type.Type ) -> String
renderTag ( tagName, tagArgs ) =
    case tagArgs of
        [] ->
            tagName

        _ ->
            tagName ++ " " ++ String.join " " (List.map Render.typeToString tagArgs)


renderAliasTui : Docs.Alias -> List Tui.Screen
renderAliasTui { name, comment, args, tipe } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] }
                ("type alias " ++ name ++ typeVars ++ " =")

        typeBodyLines =
            Render.typeToLines 6 tipe
                |> List.map
                    (\line ->
                        Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                            ("    " ++ line)
                    )

        commentLines =
            renderDocComment comment
    in
    header :: typeBodyLines ++ commentLines


renderBinopTui : Docs.Binop -> List Tui.Screen
renderBinopTui { name, comment, tipe } =
    let
        displayName =
            "(" ++ name ++ ")"

        prefixWidth =
            String.length displayName + 3

        typeLines =
            Render.typeToLines prefixWidth tipe

        header =
            case typeLines of
                [ oneLine ] ->
                    [ Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } displayName
                        , Tui.text " : "
                        , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] } oneLine
                        ]
                    ]

                multipleLines ->
                    Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } displayName
                        , Tui.text " :"
                        ]
                        :: List.map
                            (\line ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                                    ("    " ++ line)
                            )
                            multipleLines

        commentLines =
            renderDocComment comment
    in
    header ++ commentLines


renderDocComment : String -> List Tui.Screen
renderDocComment comment =
    let
        trimmed =
            String.trim comment
    in
    if String.isEmpty trimmed then
        []

    else
        Tui.text ""
            :: renderMarkdownToScreens trimmed
            |> List.map (indentScreen "    ")


{-| Indent a screen line by prepending a string. For concat/styled screens
this prepends to the first element; for text it prepends directly.
-}
indentScreen : String -> Tui.Screen -> Tui.Screen
indentScreen prefix screen =
    Tui.concat [ Tui.text prefix, screen ]



-- MARKDOWN RENDERING TO TUI SCREENS


renderMarkdownToScreens : String -> List Tui.Screen
renderMarkdownToScreens markdown =
    case
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "parse error")
            |> Result.andThen (Markdown.Renderer.render tuiRenderer)
    of
        Ok rendered ->
            List.concat rendered

        Err _ ->
            -- Fallback: raw text
            markdown
                |> String.lines
                |> List.map Tui.text


{-| The markdown renderer produces `List Tui.Screen` per block.

Inline elements (text, codeSpan, strong, link, etc.) each return a single-element
list containing one `Tui.Screen` fragment. Block elements (paragraph, heading,
list) concat all inline children into a single line via `Tui.concat`, then return
that as a `List Tui.Screen` (possibly multiple lines for lists/blockquotes).

-}
tuiRenderer : Renderer (List Tui.Screen)
tuiRenderer =
    { heading = tuiHeading
    , paragraph = tuiParagraph
    , blockQuote = tuiBlockQuote
    , html = Markdown.Html.oneOf []
    , text = \s -> [ Tui.text (String.replace "\n" " " s) ]
    , codeSpan = \code -> [ Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] } code ]
    , strong = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } (inlineChildrenToString children) ]
    , emphasis = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Italic ] } (inlineChildrenToString children) ]
    , strikethrough = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Strikethrough ] } (inlineChildrenToString children) ]
    , hardLineBreak = [ Tui.text "\n" ]
    , link = tuiLink
    , image = tuiImage
    , unorderedList = tuiUnorderedList
    , orderedList = tuiOrderedList
    , codeBlock = tuiCodeBlock
    , thematicBreak = [ Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] } (String.repeat 40 "─") ]
    , table = \children -> List.concat children
    , tableHeader = \children -> List.concat children
    , tableBody = \children -> List.concat children
    , tableRow = \children -> [ Tui.concat (List.concatMap (\c -> Tui.text "| " :: c) children ++ [ Tui.text " |" ]) ]
    , tableCell = \_ children -> List.concat children
    , tableHeaderCell = \_ children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } (inlineChildrenToString children) ]
    }


{-| Flatten inline children (from the markdown renderer) into a single plain string.
Used when we need to wrap the combined text in a single Tui.styled call.
-}
inlineChildrenToString : List (List Tui.Screen) -> String
inlineChildrenToString children =
    children
        |> List.concat
        |> List.map Tui.toString
        |> String.join ""


{-| Combine inline children into a single Tui.Screen by concatenating fragments.
This is the key to keeping paragraph text on one line instead of splitting each
inline element onto its own line.
-}
flattenInlineChildren : List (List Tui.Screen) -> Tui.Screen
flattenInlineChildren children =
    Tui.concat (List.concat children)


tuiHeading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (List Tui.Screen) } -> List Tui.Screen
tuiHeading { level, children } =
    let
        prefix =
            case level of
                Markdown.Block.H1 ->
                    "# "

                Markdown.Block.H2 ->
                    "## "

                Markdown.Block.H3 ->
                    "### "

                _ ->
                    "#### "
    in
    [ Tui.text ""
    , Tui.styled { fg = Just Ansi.Color.magenta, bg = Nothing, attributes = [ Tui.Bold ] }
        (prefix ++ inlineChildrenToString children)
    ]


tuiParagraph : List (List Tui.Screen) -> List Tui.Screen
tuiParagraph children =
    [ flattenInlineChildren children, Tui.text "" ]


tuiBlockQuote : List (List Tui.Screen) -> List Tui.Screen
tuiBlockQuote children =
    let
        allText =
            inlineChildrenToString children
    in
    allText
        |> String.lines
        |> List.map
            (\line ->
                Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] }
                    ("> " ++ line)
            )


tuiLink : { title : Maybe String, destination : String } -> List (List Tui.Screen) -> List Tui.Screen
tuiLink { destination } children =
    [ Tui.concat
        [ Tui.styled { fg = Just Ansi.Color.blue, bg = Nothing, attributes = [ Tui.Underline ] }
            (inlineChildrenToString children)
        , Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] }
            (" (" ++ destination ++ ")")
        ]
    ]


tuiImage : { alt : String, src : String, title : Maybe String } -> List Tui.Screen
tuiImage { alt, src } =
    [ Tui.concat
        [ Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [] } ("[image: " ++ alt ++ "]")
        , Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] } (" (" ++ src ++ ")")
        ]
    ]


tuiUnorderedList : List (ListItem (List Tui.Screen)) -> List Tui.Screen
tuiUnorderedList items =
    List.concatMap
        (\(ListItem task children) ->
            let
                bullet =
                    case task of
                        NoTask ->
                            Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } "  * "

                        IncompleteTask ->
                            Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [] } "  [ ] "

                        CompletedTask ->
                            Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } "  [x] "

                -- Each child is a block (usually a paragraph) which is List Tui.Screen.
                -- Flatten all children into one line for simple list items.
                childContent =
                    flattenInlineChildren children
            in
            [ Tui.concat [ bullet, childContent ] ]
        )
        items


tuiOrderedList : Int -> List (List (List Tui.Screen)) -> List Tui.Screen
tuiOrderedList startIndex items =
    List.indexedMap
        (\i children ->
            let
                num =
                    String.fromInt (startIndex + i) ++ ". "

                numScreen =
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } ("  " ++ num)

                childContent =
                    flattenInlineChildren children
            in
            [ Tui.concat [ numScreen, childContent ] ]
        )
        items
        |> List.concat


tuiCodeBlock : { body : String, language : Maybe String } -> List Tui.Screen
tuiCodeBlock { body, language } =
    let
        lang =
            language |> Maybe.withDefault "elm"

        trimmedBody =
            String.trimRight body
    in
    highlightCodeToScreens lang trimmedBody



-- SYNTAX HIGHLIGHTING TO TUI SCREENS


highlightCodeToScreens : String -> String -> List Tui.Screen
highlightCodeToScreens language body =
    let
        parser =
            case language of
                "elm" ->
                    Just SyntaxHighlight.elm

                "javascript" ->
                    Just SyntaxHighlight.javascript

                "js" ->
                    Just SyntaxHighlight.javascript

                "json" ->
                    Just SyntaxHighlight.json

                "css" ->
                    Just SyntaxHighlight.css

                "xml" ->
                    Just SyntaxHighlight.xml

                "html" ->
                    Just SyntaxHighlight.xml

                "python" ->
                    Just SyntaxHighlight.python

                "py" ->
                    Just SyntaxHighlight.python

                "sql" ->
                    Just SyntaxHighlight.sql

                "go" ->
                    Just SyntaxHighlight.go

                "nix" ->
                    Just SyntaxHighlight.nix

                "kotlin" ->
                    Just SyntaxHighlight.kotlin

                _ ->
                    Nothing

        fallback =
            body
                |> String.lines
                |> List.map
                    (\line ->
                        Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [] } line
                    )
    in
    case parser of
        Just parse ->
            case parse body of
                Ok hcode ->
                    hcode
                        |> SyntaxHighlight.toCustom tuiSyntaxTransform
                        |> List.map (\lineScreen -> Tui.concat lineScreen)

                Err _ ->
                    fallback

        Nothing ->
            fallback


tuiSyntaxTransform : SyntaxHighlight.CustomTransform Tui.Screen (List Tui.Screen)
tuiSyntaxTransform =
    { noOperation = identity
    , highlight = identity
    , addition = identity
    , deletion = identity
    , default = Tui.styled { fg = Just Ansi.Color.white, bg = Nothing, attributes = [] }
    , comment = Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] }
    , style1 = Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
    , style2 = Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] }
    , style3 = Tui.styled { fg = Just Ansi.Color.magenta, bg = Nothing, attributes = [] }
    , style4 = Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [] }
    , style5 = Tui.styled { fg = Just Ansi.Color.blue, bg = Nothing, attributes = [] }
    , style6 = Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [] }
    , style7 = Tui.styled { fg = Just Ansi.Color.brightCyan, bg = Nothing, attributes = [] }
    }



-- DIFF RENDERING


magnitudeLine : String -> Tui.Screen
magnitudeLine magnitude =
    let
        color =
            case magnitude of
                "MAJOR" ->
                    Ansi.Color.red

                "MINOR" ->
                    Ansi.Color.green

                "PATCH" ->
                    Ansi.Color.cyan

                _ ->
                    Ansi.Color.white
    in
    Tui.concat
        [ Tui.text "  "
        , Tui.styled { fg = Just Ansi.Color.white, bg = Just color, attributes = [ Tui.Bold ] }
            (" " ++ magnitude ++ " CHANGE ")
        ]


renderModuleDiff : ApiDiff -> List Docs.Module -> String -> List Tui.Screen
renderModuleDiff diff modules moduleName =
    let
        isNewModule =
            List.member moduleName diff.addedModules

        isRemovedModule =
            List.member moduleName diff.removedModules

        maybeModule =
            findModule moduleName modules
    in
    if isNewModule then
        renderNewModule moduleName maybeModule

    else if isRemovedModule then
        renderRemovedModule moduleName

    else
        renderChangedModule diff moduleName maybeModule


findModule : String -> List Docs.Module -> Maybe Docs.Module
findModule name modules =
    case modules of
        [] ->
            Nothing

        mod :: rest ->
            if mod.name == name then
                Just mod

            else
                findModule name rest


renderNewModule : String -> Maybe Docs.Module -> List Tui.Screen
renderNewModule moduleName maybeModule =
    let
        header =
            [ Tui.concat
                [ Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.green, attributes = [ Tui.Bold ] } " + "
                , Tui.text " "
                , Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ] } moduleName
                ]
            , Tui.text ""
            ]
    in
    case maybeModule of
        Just mod ->
            header ++ renderModuleDocs mod

        Nothing ->
            header


renderRemovedModule : String -> List Tui.Screen
renderRemovedModule moduleName =
    [ Tui.concat
        [ Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.red, attributes = [ Tui.Bold ] } " - "
        , Tui.text " "
        , Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold, Tui.Strikethrough ] } moduleName
        ]
    , Tui.text ""
    ]


renderChangedModule : ApiDiff -> String -> Maybe Docs.Module -> List Tui.Screen
renderChangedModule diff moduleName maybeModule =
    let
        moduleChanges =
            Diff.findModuleChanges moduleName diff.changedModules

        addedItems =
            moduleChanges
                |> Maybe.map .added
                |> Maybe.withDefault []

        changedItems =
            moduleChanges
                |> Maybe.map .changed
                |> Maybe.withDefault []

        removedItems =
            moduleChanges
                |> Maybe.map .removed
                |> Maybe.withDefault []

        addedSection =
            addedItems
                |> List.concatMap
                    (\itemName ->
                        renderItemBlock diff moduleName itemName Diff.Added maybeModule
                    )

        changedSection =
            changedItems
                |> List.concatMap
                    (\itemName ->
                        renderItemBlock diff moduleName itemName Diff.Changed maybeModule
                    )

        removedSection =
            if List.isEmpty removedItems then
                []

            else
                Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ] } "  Removed:"
                    :: List.map
                        (\item ->
                            Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Strikethrough ] } ("    " ++ item)
                        )
                        removedItems
                    ++ [ Tui.text "" ]

        commentDiffSection =
            renderCommentDiffs diff moduleName

        allSections =
            addedSection ++ changedSection ++ removedSection ++ commentDiffSection
    in
    if List.isEmpty allSections then
        [ Tui.text "  No changes in this module" ]

    else
        allSections


renderItemBlock : ApiDiff -> String -> String -> Diff.DiffStatus -> Maybe Docs.Module -> List Tui.Screen
renderItemBlock diff moduleName itemName status maybeModule =
    let
        badge =
            case status of
                Diff.Added ->
                    Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.green, attributes = [ Tui.Bold ] } " + "

                Diff.Changed ->
                    Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.yellow, attributes = [ Tui.Bold ] } " ~ "

                _ ->
                    Tui.text "   "

        oldTypeLine =
            case status of
                Diff.Changed ->
                    renderOldType diff moduleName itemName

                _ ->
                    []

        blockLines =
            case maybeModule of
                Just mod ->
                    case findBlock itemName mod of
                        Just block ->
                            renderBlock block

                        Nothing ->
                            [ Tui.concat [ badge, Tui.text (" " ++ itemName) ] ]

                Nothing ->
                    [ Tui.concat [ badge, Tui.text (" " ++ itemName) ] ]

        badgedLines =
            case blockLines of
                first :: rest ->
                    Tui.concat [ badge, first ] :: rest

                [] ->
                    [ badge ]
    in
    oldTypeLine ++ badgedLines ++ [ Tui.text "" ]


renderOldType : ApiDiff -> String -> String -> List Tui.Screen
renderOldType diff moduleName itemName =
    case Diff.getOldType diff moduleName itemName of
        Just typeValue ->
            case Decode.decodeValue Type.decoder typeValue of
                Ok tipe ->
                    [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Strikethrough ] }
                        ("   " ++ itemName ++ " : " ++ Render.typeToString tipe)
                    ]

                Err _ ->
                    []

        Nothing ->
            []


findBlock : String -> Docs.Module -> Maybe Docs.Block
findBlock name mod =
    Docs.toBlocks mod
        |> List.filter
            (\block ->
                case block of
                    Docs.ValueBlock v ->
                        v.name == name

                    Docs.UnionBlock u ->
                        u.name == name

                    Docs.AliasBlock a ->
                        a.name == name

                    Docs.BinopBlock b ->
                        b.name == name

                    _ ->
                        False
            )
        |> List.head


renderReadmeDiff : ApiDiff -> List Tui.Screen
renderReadmeDiff diff =
    case diff.readmeDiff of
        Just readmeText ->
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } "  README Changes"
                :: Tui.text ""
                :: renderUnifiedDiffLines readmeText

        Nothing ->
            [ Tui.text "  No README changes" ]


renderCommentDiffs : ApiDiff -> String -> List Tui.Screen
renderCommentDiffs diff moduleName =
    case Dict.get moduleName diff.commentDiffs of
        Just itemDiffs ->
            let
                items =
                    Dict.toList itemDiffs
            in
            if List.isEmpty items then
                []

            else
                Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } "  Doc changes:"
                    :: Tui.text ""
                    :: List.concatMap
                        (\( itemName, cdiff ) ->
                            let
                                label =
                                    if itemName == "__module__" then
                                        "Module docs"

                                    else
                                        itemName
                            in
                            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } ("    " ++ label ++ ":")
                                :: renderUnifiedDiffLines cdiff
                                ++ [ Tui.text "" ]
                        )
                        items

        Nothing ->
            []


renderUnifiedDiffLines : String -> List Tui.Screen
renderUnifiedDiffLines diffText =
    diffText
        |> String.lines
        |> List.map
            (\line ->
                if String.startsWith "+ " line || String.startsWith "+" line then
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } ("    " ++ line)

                else if String.startsWith "- " line || String.startsWith "-" line then
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [] } ("    " ++ line)

                else
                    Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] } ("    " ++ line)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Tui.Sub.Sub Msg
subscriptions model =
    Tui.Sub.batch
        ([ Tui.Sub.onKeyPress KeyPressed
         , Tui.Sub.onMouse MouseEvent
         , Tui.Sub.onContext GotContext
         ]
            ++ (if model.loadingDiff then
                    [ Tui.Spinner.subscriptions SpinnerTick ]

                else
                    []
               )
        )
