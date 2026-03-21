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
-- Tui.Input removed — using native selectableList filtering
import Tui.Keybinding as Keybinding
import Tui.Layout as Layout
import Tui.Modal as Modal
import Tui.FuzzyMatch as FuzzyMatch
import Tui.Picker
import Tui.Spinner
import Tui.Sub
import Tui.Toast as Toast


type alias Model =
    { layout : Layout.State
    , modules : List Docs.Module
    , diff : Maybe ApiDiff
    , rightView : RightView
    , showHelp : Bool
    , moduleTree : ModuleTree.ModuleTree
    , activeLeftTab : LeftTab
    -- filterInput removed — using native Layout.selectableList filtering
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    , loadingDiff : Bool
    , diffVersion : Maybe String
    , spinnerTick : Int
    , toasts : Toast.State
    , cachedDocsContent : Maybe { key : String, content : List Tui.Screen }
    , cachedRightPane : Maybe { key : String, title : String, lines : List Tui.Screen }
    , itemLinePositions : List ( String, Int )
    , internalLinks : List { line : Int, destination : String, text : String }
    , preRenderedModules : Dict.Dict String { lines : List Tui.Screen, items : List String, itemPositions : List ( String, Int ) }
    , dependencies : List String
    , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
    , readme : Maybe String
    , packagePicker : Maybe (Tui.Picker.State String)
    , comparePicker : Maybe { baseVersion : String, picker : Tui.Picker.State String }
    , browsingPackage : Maybe String
    , homeModules : List Docs.Module
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
    | ToastTick
    | GotPackageDocs String (Result FatalError (List Docs.Module))


init :
    { modules : List Docs.Module
    , diff : Maybe ApiDiff
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    , dependencies : List String
    , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
    , readme : Maybe String
    }
    -> ( Model, Effect.Effect Msg )
init { modules, diff, versions, loadDiff, dependencies, loadPackageDocs, readme } =
    ( { layout =
            Layout.init
                |> Layout.focusPane "modules"
      , modules = modules
      , diff = diff
      , rightView = DocsView
      , showHelp = False
      , moduleTree = ModuleTree.build (List.map .name modules)
      , activeLeftTab = ModulesTab
      -- filterInput removed
      , versions = versions
      , loadDiff = loadDiff
      , loadingDiff = False
      , diffVersion = Nothing
      , spinnerTick = 0
      , toasts = Toast.init
      , cachedDocsContent = Nothing
      , cachedRightPane = Nothing
      , itemLinePositions = []
      , internalLinks = []
      , preRenderedModules = Dict.empty
      , dependencies = dependencies
      , loadPackageDocs = loadPackageDocs
      , readme = readme
      , packagePicker = Nothing
      , comparePicker = Nothing
      , browsingPackage = Nothing
      , homeModules = modules
      }
    , Effect.none
    )


visibleEntries : Model -> List TreeEntry
visibleEntries model =
    let
        baseEntries =
            case model.activeLeftTab of
                ModulesTab ->
                    let
                        readmeEntry =
                            case model.readme of
                                Just _ ->
                                    [ Leaf { name = "README", depth = 0 } ]

                                Nothing ->
                                    []
                    in
                    readmeEntry ++ ModuleTree.visibleEntries model.moduleTree

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


handleKeyPressed : Tui.KeyEvent -> Model -> ( Model, Effect.Effect Msg )
handleKeyPressed event model =
    -- Pickers get first priority — all keys go to them
    case model.comparePicker of
                Just { baseVersion, picker } ->
                    case event.key of
                        Tui.Escape ->
                            ( { model | comparePicker = Nothing }, Effect.none )

                        Tui.Enter ->
                            case Tui.Picker.selected picker of
                                Just targetVersion ->
                                    let
                                        ( olderVersion, newerVersion ) =
                                            orderVersions baseVersion targetVersion

                                        diffLabel =
                                            olderVersion ++ " → " ++ newerVersion
                                    in
                                    ( { model
                                        | comparePicker = Nothing
                                        , loadingDiff = True
                                        , diffVersion = Just diffLabel
                                        , toasts = Toast.toast ("Loading diff " ++ diffLabel ++ "...") model.toasts
                                      }
                                    , Effect.perform GotDiff (model.loadDiff olderVersion)
                                    )

                                Nothing ->
                                    ( { model | comparePicker = Nothing }, Effect.none )

                        Tui.Arrow Tui.Down ->
                            ( { model | comparePicker = Just { baseVersion = baseVersion, picker = Tui.Picker.navigateDown picker } }, Effect.none )

                        Tui.Arrow Tui.Up ->
                            ( { model | comparePicker = Just { baseVersion = baseVersion, picker = Tui.Picker.navigateUp picker } }, Effect.none )

                        Tui.Backspace ->
                            ( { model | comparePicker = Just { baseVersion = baseVersion, picker = Tui.Picker.backspace picker } }, Effect.none )

                        Tui.Character c ->
                            if c == ' ' then
                                ( model, Effect.none )

                            else
                                ( { model | comparePicker = Just { baseVersion = baseVersion, picker = Tui.Picker.typeChar c picker } }, Effect.none )

                        _ ->
                            ( model, Effect.none )

                Nothing ->
                    case model.packagePicker of
                        Just picker ->
                            case event.key of
                                Tui.Escape ->
                                    ( { model | packagePicker = Nothing }, Effect.none )

                                Tui.Enter ->
                                    case Tui.Picker.selected picker of
                                        Just packageName ->
                                            ( { model
                                                | packagePicker = Nothing
                                                , loadingDiff = True
                                                , toasts = Toast.toast ("Loading " ++ packageName ++ "...") model.toasts
                                              }
                                            , Effect.attempt (GotPackageDocs packageName) (model.loadPackageDocs packageName)
                                            )

                                        Nothing ->
                                            ( { model | packagePicker = Nothing }, Effect.none )

                                Tui.Arrow Tui.Down ->
                                    ( { model | packagePicker = Just (Tui.Picker.navigateDown picker) }, Effect.none )

                                Tui.Arrow Tui.Up ->
                                    ( { model | packagePicker = Just (Tui.Picker.navigateUp picker) }, Effect.none )

                                Tui.Backspace ->
                                    ( { model | packagePicker = Just (Tui.Picker.backspace picker) }, Effect.none )

                                Tui.Character c ->
                                    if c == ' ' then
                                        ( model, Effect.none )

                                    else
                                        ( { model | packagePicker = Just (Tui.Picker.typeChar c picker) }, Effect.none )

                                _ ->
                                    ( model, Effect.none )

                        Nothing ->
                            -- Let the framework handle number keys, /, search etc.
                            case Layout.handleKeyEvent event (viewLayout (Layout.contextOf model.layout) model) model.layout of
                                ( newLayout, True ) ->
                                    ( { model | layout = newLayout }, Effect.none )

                                ( _, False ) ->
                                    case Keybinding.dispatch (allBindings model) event of
                                        Just action ->
                                            handleAction action model

                                        Nothing ->
                                            ( model, Effect.none )


update : Msg -> Model -> ( Model, Effect.Effect Msg )
update msg model =
    case msg of
        KeyPressed event ->
            handleKeyPressed event model

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
                    ( { model | layout = newLayout }
                        |> syncItemsToScroll
                    , Effect.none
                    )

        SelectEntry _ ->
            ( { model
                | layout =
                    model.layout
                        |> Layout.resetScroll "docs"
                        |> Layout.setSelectedIndex "items" 0
                , cachedDocsContent = Nothing
                , cachedRightPane = Nothing
              }
                |> refreshRightPaneCache
            , Effect.none
            )

        SelectItem idx ->
            let
                scrollTarget =
                    model.itemLinePositions
                        |> List.drop idx
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 0

                newLayout =
                    model.layout
                        |> Layout.resetScroll "docs"
                        |> Layout.scrollDown "docs" (max 0 (scrollTarget - 2))
            in
            ( { model | layout = newLayout }
            , Effect.none
            )

        GotContext ctx ->
            ( { model
                | layout =
                    Layout.withContext ctx model.layout
                , cachedRightPane = Nothing
              }
                |> refreshRightPaneCache
            , Effect.none
            )

        GotDiff maybeDiff ->
            let
                toastMessage =
                    case model.diffVersion of
                        Just version ->
                            "Diff loaded vs " ++ version

                        Nothing ->
                            "Diff loaded"
            in
            ( { model
                | diff = maybeDiff
                , loadingDiff = False
                , rightView =
                    case maybeDiff of
                        Just _ ->
                            DiffView

                        Nothing ->
                            model.rightView
                , activeLeftTab =
                    case maybeDiff of
                        Just _ ->
                            ChangesTab

                        Nothing ->
                            model.activeLeftTab
                , toasts = Toast.toast toastMessage model.toasts
                , cachedDocsContent = Nothing
                , cachedRightPane = Nothing
              }
                |> refreshRightPaneCache
            , Effect.none
            )

        SpinnerTick ->
            ( { model | spinnerTick = model.spinnerTick + 1 }, Effect.none )

        ToastTick ->
            ( { model | toasts = Toast.tick model.toasts }, Effect.none )

        GotPackageDocs packageName result ->
            case result of
                Ok newModules ->
                    ( { model
                        | modules = newModules
                        , moduleTree = ModuleTree.build (List.map .name newModules)
                        , browsingPackage = Just packageName
                        , loadingDiff = False
                        , rightView = DocsView
                        , diff = Nothing
                        , activeLeftTab = ModulesTab
                        , readme = Nothing
                        , cachedDocsContent = Nothing
                        , cachedRightPane = Nothing
                        , preRenderedModules =
                            preRenderAllModules
                                (docsPaneWidth (Layout.contextOf model.layout) - 2)
                                newModules
                        , toasts = Toast.toast ("Browsing " ++ packageName) model.toasts
                      }
                        |> refreshRightPaneCache
                    , Effect.none
                    )

                Err _ ->
                    ( { model
                        | loadingDiff = False
                        , toasts = Toast.errorToast ("Failed to load " ++ packageName) model.toasts
                      }
                    , Effect.none
                    )


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
    | CyclePaneFocus
    | CyclePaneFocusReverse
    | ScrollToSelectedItem
    | OpenPackagePicker
    | FollowLink


navigateToLink : String -> Model -> ( Model, Effect.Effect Msg )
navigateToLink destination model =
    let
        ( targetModule, targetItem ) =
            if String.startsWith "#" destination then
                ( Nothing, Just (String.dropLeft 1 destination) )

            else
                case String.split "#" destination of
                    [ moduleName, itemName ] ->
                        ( Just moduleName, Just itemName )

                    [ moduleName ] ->
                        ( Just moduleName, Nothing )

                    _ ->
                        ( Nothing, Nothing )
    in
    case targetModule of
        Just moduleName ->
            let
                entries =
                    visibleEntries model

                maybeIdx =
                    entries
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, entry ) -> ModuleTree.entryName entry == moduleName)
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            case maybeIdx of
                Just idx ->
                    let
                        newModel =
                            { model
                                | layout =
                                    model.layout
                                        |> Layout.setSelectedIndex "modules" idx
                                        |> Layout.resetScroll "docs"
                                        |> Layout.setSelectedIndex "items" 0
                                , cachedDocsContent = Nothing
                                , cachedRightPane = Nothing
                            }
                                |> refreshRightPaneCache

                        finalModel =
                            case targetItem of
                                Just item ->
                                    let
                                        scrollTarget =
                                            newModel.itemLinePositions
                                                |> List.filter (\( name, _ ) -> name == item)
                                                |> List.head
                                                |> Maybe.map Tuple.second
                                                |> Maybe.withDefault 0
                                    in
                                    { newModel
                                        | layout =
                                            newModel.layout
                                                |> Layout.resetScroll "docs"
                                                |> Layout.scrollDown "docs" (max 0 (scrollTarget - 2))
                                    }

                                Nothing ->
                                    newModel
                    in
                    ( finalModel
                        |> (\m -> { m | toasts = Toast.toast ("→ " ++ moduleName) m.toasts })
                    , Effect.none
                    )

                Nothing ->
                    ( { model | toasts = Toast.errorToast ("Module " ++ moduleName ++ " not found") model.toasts }
                    , Effect.none
                    )

        Nothing ->
            case targetItem of
                Just item ->
                    let
                        scrollTarget =
                            model.itemLinePositions
                                |> List.filter (\( name, _ ) -> name == item)
                                |> List.head
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault 0
                    in
                    ( { model
                        | layout =
                            model.layout
                                |> Layout.resetScroll "docs"
                                |> Layout.scrollDown "docs" (max 0 (scrollTarget - 2))
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )


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
            ( { model | layout = cappedScrollDown model 3 }
                |> syncItemsToScroll
            , Effect.none
            )

        ScrollDocsUp ->
            ( { model | layout = Layout.scrollUp "docs" 3 model.layout }
                |> syncItemsToScroll
            , Effect.none
            )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Effect.none )

        CloseHelp ->
            ( { model | showHelp = False }, Effect.none )

        ToggleDiffView ->
            if model.loadingDiff then
                ( model, Effect.none )

            else if model.activeLeftTab == VersionsTab then
                -- On versions tab: open compare-against picker
                case selectedEntry model of
                    Just (Leaf { name }) ->
                        let
                            otherVersions =
                                "HEAD"
                                    :: List.filter (\v -> v /= name) model.versions

                            picker =
                                Tui.Picker.open
                                    { items = otherVersions
                                    , toString = identity
                                    , title = "Compare " ++ name ++ " against"
                                    }
                        in
                        ( { model | comparePicker = Just { baseVersion = name, picker = picker } }
                        , Effect.none
                        )

                    _ ->
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
                        ( { model | rightView = newView, cachedRightPane = Nothing }
                            |> refreshRightPaneCache
                        , Effect.none
                        )

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
            ( { model | activeLeftTab = ModulesTab, rightView = DocsView, cachedRightPane = Nothing }
                |> refreshRightPaneCache
            , Effect.none
            )

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
            ( model, Effect.none )

        CancelFilter ->
            ( model, Effect.none )

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
            ( { model | layout = cappedScrollDown model pageSize }
                |> syncItemsToScroll
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
                |> syncItemsToScroll
            , Effect.none
            )

        ToggleMaximize ->
            ( { model | layout = Layout.toggleMaximize "docs" model.layout }
            , Effect.none
            )

        ScrollToSelectedItem ->
            let
                items =
                    itemsForCurrentView model

                selectedIdx =
                    Layout.selectedIndex "items" model.layout

                maybeItemName =
                    items |> List.drop selectedIdx |> List.head

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
                        |> Layout.scrollDown "docs" (max 0 (scrollTarget - 2))
            in
            ( { model | layout = newLayout }
            , Effect.none
            )

        CyclePaneFocus ->
            let
                nextPane =
                    case Layout.focusedPane model.layout of
                        Just "modules" ->
                            "items"

                        Just "items" ->
                            "docs"

                        Just "docs" ->
                            "modules"

                        _ ->
                            "modules"
            in
            ( { model | layout = Layout.focusPane nextPane model.layout }
            , Effect.none
            )

        FollowLink ->
            let
                scrollOffset =
                    Layout.scrollPosition "docs" model.layout

                ctx =
                    Layout.contextOf model.layout

                visibleTop =
                    scrollOffset

                visibleBottom =
                    scrollOffset + ctx.height - 4

                -- Extract links lazily (only when user presses Enter)
                links =
                    case selectedModule model of
                        Just mod ->
                            let
                                rawLinks =
                                    extractLinksFromComment mod.comment

                                docsContent =
                                    currentDocsContent model
                            in
                            rawLinks
                                |> List.filterMap
                                    (\link ->
                                        let
                                            linePos =
                                                findLinkLine link.text docsContent
                                        in
                                        if linePos >= 0 then
                                            Just { line = linePos, destination = link.destination, text = link.text }

                                        else
                                            Nothing
                                    )

                        Nothing ->
                            []

                visibleLink =
                    links
                        |> List.filter (\link -> link.line >= visibleTop && link.line <= visibleBottom)
                        |> List.head
            in
            case visibleLink of
                Just link ->
                    navigateToLink link.destination model

                Nothing ->
                    ( model, Effect.none )

        OpenPackagePicker ->
            if List.isEmpty model.dependencies then
                ( model, Effect.none )

            else
                ( { model
                    | packagePicker =
                        Just
                            (Tui.Picker.open
                                { items = model.dependencies
                                , toString = identity
                                , title = "Browse Package"
                                }
                            )
                  }
                , Effect.none
                )

        CyclePaneFocusReverse ->
            let
                prevPane =
                    case Layout.focusedPane model.layout of
                        Just "modules" ->
                            "docs"

                        Just "items" ->
                            "modules"

                        Just "docs" ->
                            "items"

                        _ ->
                            "modules"
            in
            ( { model | layout = Layout.focusPane prevPane model.layout }
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
                [ Keybinding.binding (Tui.Character 'c') "Changes" SwitchToChangesTab ]

            else
                []

        versionsTabBinding =
            if hasVersions then
                [ Keybinding.binding (Tui.Character 'v') "Versions" SwitchToVersionsTab ]

            else
                []

        cycleBinding =
            [ Keybinding.binding Tui.Tab "Cycle panes" CyclePaneFocus
            , Keybinding.withModifiers [ Tui.Shift ] Tui.Tab "Prev pane" CyclePaneFocusReverse
            ]
    in
    Keybinding.group "Global"
        ([ Keybinding.binding (Tui.Character 'q') "Quit" Quit
         , Keybinding.binding Tui.Escape "Quit" Quit
         , Keybinding.binding (Tui.Character '?') "Toggle help" ToggleHelp
         , Keybinding.binding (Tui.Character 'h') "Focus left" FocusLeft
             |> Keybinding.withAlternate (Tui.Arrow Tui.Left)
         , Keybinding.binding (Tui.Character 'l') "Focus right" FocusRight
             |> Keybinding.withAlternate (Tui.Arrow Tui.Right)
         ]
            ++ diffToggleBinding
            ++ changesTabBinding
            ++ versionsTabBinding
            ++ cycleBinding
            ++ (if not (List.isEmpty model.dependencies) then
                    [ Keybinding.binding (Tui.Character 'p') "Browse package" OpenPackagePicker ]

                else
                    []
               )
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
        , Keybinding.binding Tui.Enter "Jump to item" ScrollToSelectedItem
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
        , Keybinding.binding Tui.Enter "Follow link" FollowLink
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

        toastView =
            Toast.view model.toasts

        layoutRows =
            Layout.toRows layoutState (viewLayout contentCtx model)
    in
    case model.comparePicker of
        Just { picker } ->
            let
                baseRows =
                    layoutRows ++ [ statusBar ]
            in
            Modal.overlay
                { title = Tui.Picker.title picker
                , body = Tui.Picker.viewBody picker
                , footer = String.fromInt (Tui.Picker.matchCount picker) ++ " versions  Enter select  Esc cancel"
                , width = min 50 (ctx.width - 4)
                }
                { width = ctx.width, height = ctx.height - 1 }
                baseRows
                |> Tui.lines

        Nothing ->
            case model.packagePicker of
                Just picker ->
                    let
                        baseRows =
                            layoutRows ++ [ statusBar ]
                    in
                    Modal.overlay
                        { title = Tui.Picker.title picker
                        , body = Tui.Picker.viewBody picker
                        , footer = String.fromInt (Tui.Picker.matchCount picker) ++ " packages  type to filter  Enter select  Esc cancel"
                        , width = min 60 (ctx.width - 4)
                        }
                        { width = ctx.width, height = ctx.height - 1 }
                        baseRows
                        |> Tui.lines

                Nothing ->
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
                            { width = ctx.width, height = ctx.height - 1 }
                            layoutRows
                            ++ [ statusBar ]
                            |> Tui.lines

                    else if Toast.hasToasts model.toasts then
                        (layoutRows ++ [ Tui.concat [ statusBar, Tui.text " ", toastView ] ])
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
                    let
                        base =
                            [ "j/k navigate", "? help", "q quit" ]

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
            , hyperlink = Nothing
            }
    in
    -- Show filter/search status bar if active on any pane
    case Layout.activeFilterStatusBar model.layout of
        Just filterBar ->
            filterBar

        Nothing ->
            Tui.styled barStyle (hintText ++ String.repeat padding " ")


modulesPaneWidth : Int -> Layout.Width
modulesPaneWidth termWidth =
    Layout.fixed (min 28 (termWidth // 4))


leftPaneTitle : Model -> String
leftPaneTitle model =
    "Modules"


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
                    |> Layout.withFilterable ModuleTree.entryName entries
                )

    in
    pane
        -- Entry count shown in footer, not prefix (framework handles [1] tab number)
        |> Layout.withFooter
            (String.fromInt (Layout.selectedIndex "modules" model.layout + 1)
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
            case selectedEntry model of
                Just (Leaf { name }) ->
                    if name == "README" then
                        case model.readme of
                            Just readmeContent ->
                                extractHeadings readmeContent

                            Nothing ->
                                []

                    else
                        case selectedModule model of
                            Just mod ->
                                moduleItemNames mod

                            Nothing ->
                                []

                _ ->
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
        , width = Layout.fixed 20
        }
        (Layout.selectableList
            { onSelect = SelectItem
            , selected =
                \name ->
                    Tui.styled
                        { fg = Just Ansi.Color.white
                        , bg = Just Ansi.Color.blue
                        , attributes = [ Tui.Bold ]
                        , hyperlink = Nothing
                        }
                        (name ++ " ")
            , default =
                \name ->
                    if String.startsWith "# " name then
                        Tui.text name |> Tui.dim |> Tui.bold

                    else
                        Tui.text name
            }
            items
            |> Layout.withFilterable identity items
        )
        |> Layout.withFooter
            (if itemCount > 0 then
                String.fromInt (Layout.selectedIndex "items" model.layout + 1)
                    ++ " of "
                    ++ String.fromInt itemCount

             else
                ""
            )


rightPaneCacheKey : Int -> Model -> String
rightPaneCacheKey wrapWidth model =
    docsContentCacheKey model ++ ":" ++ String.fromInt wrapWidth


syncItemsToScroll : Model -> Model
syncItemsToScroll model =
    if List.isEmpty model.itemLinePositions then
        model

    else
    let
        scrollOffset =
            Layout.scrollPosition "docs" model.layout

        anchor =
            scrollOffset + 3

        bestIndex =
            model.itemLinePositions
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( idx, ( _, linePos ) ) best ->
                        if linePos <= anchor then
                            idx

                        else
                            best
                    )
                    0
    in
    { model | layout = Layout.setSelectedIndex "items" bestIndex model.layout }


extractInternalLinks : List Tui.Screen -> List { line : Int, destination : String, text : String }
extractInternalLinks lines =
    lines
        |> List.indexedMap
            (\idx line ->
                let
                    text =
                        Tui.toString line
                in
                -- Look for magenta underlined text (our internal link style)
                -- We can't perfectly detect styled regions from toString,
                -- but we can scan the module comment for link destinations
                -- For now, return empty - will populate from module comment
                []
            )
        |> List.concat


findLinkLine : String -> List Tui.Screen -> Int
findLinkLine linkText lines =
    lines
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, line ) -> String.contains linkText (Tui.toString line))
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1


extractLinksFromComment : String -> List { destination : String, text : String }
extractLinksFromComment comment =
    -- Parse markdown links: [text](destination) where destination doesn't start with http
    comment
        |> String.split "["
        |> List.drop 1
        |> List.filterMap
            (\segment ->
                case String.split "](" segment of
                    linkText :: rest :: _ ->
                        let
                            destination =
                                rest
                                    |> String.split ")"
                                    |> List.head
                                    |> Maybe.withDefault ""
                        in
                        if String.startsWith "http" destination || String.isEmpty destination then
                            Nothing

                        else
                            Just { destination = destination, text = linkText }

                    _ ->
                        Nothing
            )


preRenderAllModules : Int -> List Docs.Module -> Dict.Dict String { lines : List Tui.Screen, items : List String, itemPositions : List ( String, Int ) }
preRenderAllModules wrapWidth modules =
    modules
        |> List.map
            (\mod ->
                let
                    rendered =
                        renderModuleDocsWithPositions wrapWidth mod

                    items =
                        moduleItemNames mod
                in
                ( mod.name
                , { lines = rendered.lines
                  , items = items
                  , itemPositions = rendered.positions
                  }
                )
            )
        |> Dict.fromList


refreshRightPaneCache : Model -> Model
refreshRightPaneCache model =
    let
        ctx =
            Layout.contextOf model.layout

        wrapWidth =
            docsPaneWidth ctx - 2

        key =
            rightPaneCacheKey wrapWidth model
    in
    let
        buildAndCachePositions m =
            let
                -- Check if we have a pre-rendered version
                selectedName =
                    case selectedEntry m of
                        Just (Leaf { name }) ->
                            name

                        _ ->
                            ""

                preRendered =
                    Dict.get selectedName m.preRenderedModules
            in
            case preRendered of
                Just rendered ->
                    -- Use pre-rendered content (instant!)
                    { m
                        | cachedRightPane =
                            Just
                                { key = key
                                , title = "Docs: " ++ selectedName
                                , lines = rendered.lines
                                }
                        , itemLinePositions = rendered.itemPositions
                        , internalLinks = []
                    }

                Nothing ->
                    -- Fall back to rendering on demand
                    let
                        fallbackName =
                            case selectedEntry m of
                                Just (Leaf { name }) ->
                                    name

                                _ ->
                                    ""

                        ( cacheEntry, positions ) =
                            if m.rightView == DocsView && fallbackName /= "README" && fallbackName /= "" then
                                case findModule fallbackName m.modules of
                                    Just mod ->
                                        let
                                            rendered =
                                                renderModuleDocsWithPositions wrapWidth mod
                                        in
                                        ( { key = key
                                          , title = "Docs: " ++ fallbackName
                                          , lines = rendered.lines
                                          }
                                        , rendered.positions
                                        )

                                    Nothing ->
                                        ( buildRightPaneCache wrapWidth m key, [] )

                            else
                                ( buildRightPaneCache wrapWidth m key
                                , findAllItemLines (itemsForCurrentView m) (buildRightPaneCache wrapWidth m key).lines
                                )
                    in
                    { m
                        | cachedRightPane = Just cacheEntry
                        , itemLinePositions = positions
                        , internalLinks = []
                    }
    in
    case model.cachedRightPane of
        Just cached ->
            if cached.key == key then
                model

            else
                buildAndCachePositions model

        Nothing ->
            buildAndCachePositions model


buildRightPaneCache : Int -> Model -> String -> { key : String, title : String, lines : List Tui.Screen }
buildRightPaneCache wrapWidth model key =
    case model.rightView of
        DocsView ->
            case selectedEntry model of
                Just (Leaf { name }) ->
                    if name == "README" then
                        case model.readme of
                            Just readmeContent ->
                                { key = key
                                , title = "README"
                                , lines =
                                    renderMarkdownToScreens readmeContent
                                        |> wrapLines wrapWidth
                                }

                            Nothing ->
                                { key = key
                                , title = "README"
                                , lines = [ Tui.text "No README found" ]
                                }

                    else
                        case findModule name model.modules of
                            Just mod ->
                                { key = key
                                , title = "Docs: " ++ name
                                , lines = renderModuleDocs wrapWidth mod
                                }

                            Nothing ->
                                { key = key
                                , title = "Docs: " ++ name
                                , lines = [ Tui.text ("Module " ++ name ++ " not found") ]
                                }

                Just (Group { prefix }) ->
                    { key = key
                    , title = "Docs: " ++ prefix
                    , lines = [ Tui.text ("Select a module under " ++ prefix) ]
                    }

                Nothing ->
                    { key = key
                    , title = "Docs"
                    , lines = [ Tui.text "No module selected" ]
                    }

        DiffView ->
            let
                diffTitle =
                    case model.diffVersion of
                        Just version ->
                            "Diff vs " ++ version

                        Nothing ->
                            "Diff"

                diffLines =
                    case model.diff of
                        Just diff ->
                            let
                                magnitudeHeader =
                                    [ magnitudeLine diff.magnitude
                                    , changeSummaryLine diff
                                    , Tui.text ""
                                    ]

                                entryDiffLines =
                                    case selectedEntry model of
                                        Just (Leaf { name }) ->
                                            if name == "README" then
                                                renderReadmeDiff diff

                                            else
                                                renderModuleDiff wrapWidth diff model.modules name

                                        _ ->
                                            [ Tui.text "No module selected" ]
                            in
                            magnitudeHeader ++ entryDiffLines

                        Nothing ->
                            [ Tui.text "No diff data" ]
            in
            { key = key
            , title = diffTitle
            , lines = diffLines
            }


orderVersions : String -> String -> ( String, String )
orderVersions a b =
    if a == "HEAD" then
        -- HEAD is always newer
        ( b, "HEAD" )

    else if b == "HEAD" then
        ( a, "HEAD" )

    else
        -- Compare semver: older version first
        case compareVersionStrings a b of
            GT ->
                ( b, a )

            _ ->
                ( a, b )


compareVersionStrings : String -> String -> Order
compareVersionStrings a b =
    let
        toInts s =
            s |> String.split "." |> List.filterMap String.toInt
    in
    compareLists (toInts a) (toInts b)


compareLists : List Int -> List Int -> Order
compareLists a b =
    case ( a, b ) of
        ( [], [] ) ->
            EQ

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT

        ( x :: xs, y :: ys ) ->
            case compare x y of
                EQ ->
                    compareLists xs ys

                other ->
                    other


docsContentCacheKey : Model -> String
docsContentCacheKey model =
    let
        entryName =
            case selectedEntry model of
                Just (Leaf { name }) ->
                    name

                Just (Group { prefix }) ->
                    prefix

                Nothing ->
                    ""

        viewKey =
            case model.rightView of
                DocsView ->
                    "docs"

                DiffView ->
                    "diff"
    in
    viewKey ++ ":" ++ entryName


currentDocsContent : Model -> List Tui.Screen
currentDocsContent model =
    let
        key =
            docsContentCacheKey model
    in
    case model.cachedDocsContent of
        Just cached ->
            if cached.key == key then
                cached.content

            else
                buildDocsContent model

        Nothing ->
            buildDocsContent model


buildDocsContent : Model -> List Tui.Screen
buildDocsContent model =
    let
        width =
            (Layout.contextOf model.layout).width
    in
    case model.rightView of
        DocsView ->
            case selectedModule model of
                Just mod ->
                    renderModuleDocs width mod

                Nothing ->
                    []

        DiffView ->
            case ( model.diff, selectedEntry model ) of
                ( Just diff, Just (Leaf { name }) ) ->
                    if name == "README" then
                        renderReadmeDiff diff

                    else
                        [ magnitudeLine diff.magnitude, Tui.text "" ]
                            ++ renderModuleDiff width diff model.modules name

                _ ->
                    []


findAllItemLines : List String -> List Tui.Screen -> List ( String, Int )
findAllItemLines items lines =
    let
        -- Build a set of items we're looking for (for quick membership check)
        itemSet =
            items

        -- Pre-convert all lines to strings once
        indexedTextLines =
            lines
                |> List.indexedMap (\idx line -> ( idx, Tui.toString line ))

        -- For each item, find its line position
        findItem itemName =
            let
                headingText =
                    String.dropLeft 2 itemName

                matchLine ( _, text ) =
                    if String.startsWith "# " itemName then
                        -- Match heading: look for the heading text in rendered content
                        -- Use the first significant words to avoid partial matches
                        String.contains headingText text
                            && String.contains "##" text

                    else
                        String.contains ("┃ " ++ itemName ++ " :") text
                            || String.contains ("┃ " ++ itemName ++ " =") text
                            || String.contains ("┃ type " ++ itemName ++ " ") text
                            || String.contains ("┃ type alias " ++ itemName ++ " ") text
                            || String.endsWith ("┃ type " ++ itemName) text
                            || String.endsWith ("┃ type alias " ++ itemName) text
            in
            indexedTextLines
                |> List.filter matchLine
                |> List.head
                |> Maybe.map (\( idx, _ ) -> ( itemName, idx ))
                |> Maybe.withDefault ( itemName, 0 )
    in
    List.map findItem itemSet


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
                if String.startsWith "# " itemName then
                    -- Section heading: look for heading text in rendered content
                    String.contains (String.dropLeft 2 itemName) text
                        && String.contains "##" text

                else
                    -- Match definitions: item name must appear right after the gutter
                    -- "┃ xs : Radius" matches but "┃     { xs : Float" does not
                    String.contains ("┃ " ++ itemName ++ " :") text
                        || String.contains ("┃ " ++ itemName ++ " =") text
                        || String.contains ("┃ type " ++ itemName ++ " ") text
                        || String.contains ("┃ type alias " ++ itemName ++ " ") text
                        || String.endsWith ("┃ type " ++ itemName) text
                        || String.endsWith ("┃ type alias " ++ itemName) text
            )
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


moduleItemNames : Docs.Module -> List String
moduleItemNames mod =
    Docs.toBlocks mod
        |> List.concatMap
            (\block ->
                case block of
                    Docs.ValueBlock v ->
                        [ v.name ]

                    Docs.UnionBlock u ->
                        [ u.name ]

                    Docs.AliasBlock a ->
                        [ a.name ]

                    Docs.BinopBlock b ->
                        [ b.name ]

                    Docs.MarkdownBlock markdown ->
                        extractHeadings markdown

                    _ ->
                        []
            )


extractHeadings : String -> List String
extractHeadings markdown =
    markdown
        |> String.trim
        |> String.lines
        |> List.filterMap
            (\line ->
                if String.startsWith "## " (String.trim line) then
                    Just ("# " ++ stripMarkdownInline (String.trim (String.dropLeft 3 (String.trim line))))

                else
                    Nothing
            )


{-| Strip inline markdown formatting from text.
Removes backticks, link syntax [text](url) → text, and bold/italic markers.
-}
stripMarkdownInline : String -> String
stripMarkdownInline text =
    text
        |> stripLinks
        |> String.replace "`" ""
        |> String.replace "**" ""
        |> String.replace "__" ""


{-| Strip markdown link syntax: [text](url) → text
-}
stripLinks : String -> String
stripLinks text =
    case String.split "[" text of
        [] ->
            text

        first :: rest ->
            first
                ++ String.join ""
                    (List.map
                        (\segment ->
                            case String.split "](" segment of
                                linkText :: urlParts :: _ ->
                                    let
                                        afterUrl =
                                            urlParts
                                                |> String.split ")"
                                                |> List.drop 1
                                                |> String.join ")"
                                    in
                                    linkText ++ afterUrl

                                _ ->
                                    "[" ++ segment
                        )
                        rest
                    )


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
            , hyperlink = Nothing
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
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " + "

                Diff.ModuleChanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "

                Diff.ModuleRemoved ->
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " - "

                Diff.ModuleUnchanged ->
                    Tui.text "   "

        Nothing ->
            Tui.empty



-- RIGHT PANE


docsPaneWidth : { a | width : Int } -> Int
docsPaneWidth ctx =
    let
        modulesWidth =
            min 28 (ctx.width // 4)

        itemsWidth =
            20

        borders =
            6
    in
    max 20 (ctx.width - modulesWidth - itemsWidth - borders)


rightPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
rightPane ctx model =
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
        let
            wrapWidth =
                docsPaneWidth ctx - 2

            -- Check if cache is fresh; if not, use it anyway but it'll be refreshed next update
            cached =
                case model.cachedRightPane of
                    Just c ->
                        if c.key == rightPaneCacheKey wrapWidth model then
                            c

                        else
                            buildRightPaneCache wrapWidth model (rightPaneCacheKey wrapWidth model)

                    Nothing ->
                        buildRightPaneCache wrapWidth model (rightPaneCacheKey wrapWidth model)
        in
        Layout.pane "docs"
            { title = cached.title
            , width = Layout.fill
            }
            (Layout.content cached.lines
                |> Layout.withSearchable
            )
            |> withScrollPercentFooter model cached.lines



cappedScrollDown : Model -> Int -> Layout.State
cappedScrollDown model delta =
    let
        ctx =
            Layout.contextOf model.layout

        contentLines =
            List.length (currentDocsContent model)

        visibleHeight =
            ctx.height - 2

        currentOffset =
            Layout.scrollPosition "docs" model.layout

        maxOffset =
            max 0 (contentLines - visibleHeight)

        clampedDelta =
            min delta (max 0 (maxOffset - currentOffset))
    in
    Layout.scrollDown "docs" clampedDelta model.layout


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
                (Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing } percent)



wrapLines : Int -> List Tui.Screen -> List Tui.Screen
wrapLines maxWidth lines =
    if maxWidth <= 0 then
        lines

    else
        List.concatMap (Tui.wrapWidth maxWidth) lines



-- DOCS RENDERING


renderModuleDocs : Int -> Docs.Module -> List Tui.Screen
renderModuleDocs wrapWidth mod =
    renderModuleDocsWithGutter wrapWidth Ansi.Color.brightBlack mod


{-| Render module docs and return both lines AND item positions.
Positions are computed by counting lines during rendering — no string searching.
-}
renderModuleDocsWithPositions : Int -> Docs.Module -> { lines : List Tui.Screen, positions : List ( String, Int ) }
renderModuleDocsWithPositions wrapWidth mod =
    let
        blocks =
            Docs.toBlocks mod

        separator =
            [ Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
                (String.repeat 40 "─")
            , Tui.text ""
            ]

        -- Fold over blocks, accumulating lines and positions with running line count
        result =
            List.foldl
                (\block acc ->
                    let
                        blockLines =
                            renderBlock wrapWidth Ansi.Color.brightBlack block

                        prefixLines =
                            case block of
                                Docs.MarkdownBlock _ ->
                                    []

                                _ ->
                                    separator

                        allBlockLines =
                            prefixLines ++ blockLines ++ [ Tui.text "" ]

                        blockStart =
                            acc.lineCount + List.length prefixLines

                        itemName =
                            case block of
                                Docs.ValueBlock v ->
                                    Just v.name

                                Docs.UnionBlock u ->
                                    Just u.name

                                Docs.AliasBlock a ->
                                    Just a.name

                                Docs.BinopBlock b ->
                                    Just b.name

                                Docs.MarkdownBlock markdown ->
                                    extractHeadings markdown
                                        |> List.head

                                _ ->
                                    Nothing

                        newPositions =
                            case block of
                                Docs.MarkdownBlock markdown ->
                                    -- Markdown blocks can have multiple headings
                                    let
                                        headings =
                                            extractHeadings markdown
                                    in
                                    case headings of
                                        [] ->
                                            acc.positions

                                        _ ->
                                            -- For multiple headings in one block, first heading gets this block's position
                                            -- (we can't easily split a single MarkdownBlock's rendered lines by heading)
                                            List.foldl
                                                (\heading posAcc ->
                                                    posAcc ++ [ ( heading, acc.lineCount ) ]
                                                )
                                                acc.positions
                                                headings

                                _ ->
                                    case itemName of
                                        Just name ->
                                            acc.positions ++ [ ( name, blockStart ) ]

                                        Nothing ->
                                            acc.positions
                    in
                    { lineCount = acc.lineCount + List.length allBlockLines
                    , lines = acc.lines ++ allBlockLines
                    , positions = newPositions
                    }
                )
                { lineCount = 0, lines = [], positions = [] }
                blocks
    in
    { lines = result.lines, positions = result.positions }


renderModuleDocsWithGutter : Int -> Ansi.Color.Color -> Docs.Module -> List Tui.Screen
renderModuleDocsWithGutter wrapWidth gutterColor mod =
    let
        blocks =
            Docs.toBlocks mod

        separator =
            [ Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
                (String.repeat 40 "─")
            , Tui.text ""
            ]
    in
    blocks
        |> List.concatMap
            (\block ->
                case block of
                    Docs.MarkdownBlock _ ->
                        renderBlock wrapWidth gutterColor block ++ [ Tui.text "" ]

                    _ ->
                        separator ++ renderBlock wrapWidth gutterColor block ++ [ Tui.text "" ]
            )


renderBlock : Int -> Ansi.Color.Color -> Docs.Block -> List Tui.Screen
renderBlock wrapWidth gutterColor block =
    case block of
        Docs.ValueBlock value ->
            renderValueTui wrapWidth value
                |> addGutter gutterColor

        Docs.UnionBlock union ->
            renderUnionTui wrapWidth union
                |> addGutter gutterColor

        Docs.AliasBlock alias_ ->
            renderAliasTui wrapWidth alias_
                |> addGutter gutterColor

        Docs.BinopBlock binop ->
            renderBinopTui wrapWidth binop
                |> addGutter gutterColor

        Docs.MarkdownBlock markdown ->
            renderMarkdownToScreens (String.trim markdown)
                |> wrapLines wrapWidth

        Docs.UnknownBlock name ->
            [ Tui.text ("  (unknown: " ++ name ++ ")") ]


addGutter : Ansi.Color.Color -> List Tui.Screen -> List Tui.Screen
addGutter color lines =
    let
        gutterChar =
            Tui.styled { fg = Just color, bg = Nothing, attributes = [], hyperlink = Nothing } "┃ "
    in
    List.map (\line -> Tui.concat [ gutterChar, line ]) lines


renderValueTui : Int -> Docs.Value -> List Tui.Screen
renderValueTui wrapWidth { name, comment, tipe } =
    let
        prefixWidth =
            String.length name + 3

        typeLines =
            Render.typeToLines prefixWidth tipe

        header =
            case typeLines of
                [ oneLine ] ->
                    [ Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } name
                        , Tui.text " : "
                        , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing } oneLine
                        ]
                    ]

                multipleLines ->
                    Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } name
                        , Tui.text " :"
                        ]
                        :: List.map
                            (\line ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
                                    ("    " ++ line)
                            )
                            multipleLines

        commentLines =
            renderDocComment wrapWidth comment
    in
    header ++ commentLines


renderUnionTui : Int -> Docs.Union -> List Tui.Screen
renderUnionTui wrapWidth { name, comment, args, tags } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
                ("type " ++ name ++ typeVars)

        constructorLines =
            case tags of
                [] ->
                    []

                first :: rest ->
                    Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
                        ("    = " ++ renderTag first)
                        :: List.map
                            (\t ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
                                    ("    | " ++ renderTag t)
                            )
                            rest

        commentLines =
            renderDocComment wrapWidth comment
    in
    header :: constructorLines ++ commentLines


renderTag : ( String, List Type.Type ) -> String
renderTag ( tagName, tagArgs ) =
    case tagArgs of
        [] ->
            tagName

        _ ->
            tagName ++ " " ++ String.join " " (List.map Render.typeToString tagArgs)


renderAliasTui : Int -> Docs.Alias -> List Tui.Screen
renderAliasTui wrapWidth { name, comment, args, tipe } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
                ("type alias " ++ name ++ typeVars ++ " =")

        typeBodyLines =
            Render.typeToLines 6 tipe
                |> List.map
                    (\line ->
                        Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
                            ("    " ++ line)
                    )

        commentLines =
            renderDocComment wrapWidth comment
    in
    header :: typeBodyLines ++ commentLines


renderBinopTui : Int -> Docs.Binop -> List Tui.Screen
renderBinopTui wrapWidth { name, comment, tipe } =
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
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } displayName
                        , Tui.text " : "
                        , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing } oneLine
                        ]
                    ]

                multipleLines ->
                    Tui.concat
                        [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } displayName
                        , Tui.text " :"
                        ]
                        :: List.map
                            (\line ->
                                Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
                                    ("    " ++ line)
                            )
                            multipleLines

        commentLines =
            renderDocComment wrapWidth comment
    in
    header ++ commentLines


renderDocComment : Int -> String -> List Tui.Screen
renderDocComment wrapWidth comment =
    let
        trimmed =
            String.trim comment

        commentWidth =
            wrapWidth - 6

        needsMarkdown =
            String.contains "`" trimmed
                || String.contains "[" trimmed
                || String.contains "#" trimmed
                || String.contains "*" trimmed
                || String.contains "    " trimmed
                || String.contains "- " trimmed
    in
    if String.isEmpty trimmed then
        []

    else if needsMarkdown then
        Tui.text ""
            :: (renderMarkdownToScreens trimmed
                    |> wrapLines commentWidth
               )
            |> List.map (indentScreen "    ")

    else
        -- Fast path: plain text, no markdown parsing needed
        Tui.text ""
            :: (trimmed
                    |> String.lines
                    |> List.map Tui.text
                    |> wrapLines commentWidth
               )
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
    , codeSpan = \code -> [ Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing } code ]
    , strong = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } (inlineChildrenToString children) ]
    , emphasis = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Italic ], hyperlink = Nothing } (inlineChildrenToString children) ]
    , strikethrough = \children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Strikethrough ], hyperlink = Nothing } (inlineChildrenToString children) ]
    , hardLineBreak = [ Tui.text "\n" ]
    , link = tuiLink
    , image = tuiImage
    , unorderedList = tuiUnorderedList
    , orderedList = tuiOrderedList
    , codeBlock = tuiCodeBlock
    , thematicBreak = [ Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing } (String.repeat 40 "─") ]
    , table = \children -> List.concat children
    , tableHeader = \children -> List.concat children
    , tableBody = \children -> List.concat children
    , tableRow = \children -> [ Tui.concat (List.concatMap (\c -> Tui.text "| " :: c) children ++ [ Tui.text " |" ]) ]
    , tableCell = \_ children -> List.concat children
    , tableHeaderCell = \_ children -> [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } (inlineChildrenToString children) ]
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
    , Tui.styled { fg = Just Ansi.Color.magenta, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
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
                Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
                    ("> " ++ line)
            )


tuiLink : { title : Maybe String, destination : String } -> List (List Tui.Screen) -> List Tui.Screen
tuiLink { destination } children =
    let
        linkText =
            inlineChildrenToString children

        isExternal =
            String.startsWith "http://" destination
                || String.startsWith "https://" destination
    in
    if isExternal then
        [ Tui.text linkText
            |> Tui.underline
            |> Tui.fg Ansi.Color.blue
            |> Tui.link { url = destination }
        ]

    else
        -- Internal Elm doc link: #definition or Module#definition
        -- Use magenta + underline to distinguish from code spans (cyan)
        [ Tui.text linkText
            |> Tui.fg Ansi.Color.magenta
            |> Tui.underline
        ]


tuiImage : { alt : String, src : String, title : Maybe String } -> List Tui.Screen
tuiImage { alt, src } =
    [ Tui.concat
        [ Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [], hyperlink = Nothing } ("[image: " ++ alt ++ "]")
        , Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing } (" (" ++ src ++ ")")
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
                            Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing } "  * "

                        IncompleteTask ->
                            Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [], hyperlink = Nothing } "  [ ] "

                        CompletedTask ->
                            Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing } "  [x] "

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
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing } ("  " ++ num)

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

        codeLines =
            highlightCodeToScreens lang trimmedBody
                |> List.map (\line -> Tui.concat [ Tui.text "  ", line ])
    in
    codeLines ++ [ Tui.text "" ]



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
                        Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [], hyperlink = Nothing } line
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
    , default = Tui.styled { fg = Just Ansi.Color.white, bg = Nothing, attributes = [], hyperlink = Nothing }
    , comment = Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style1 = Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style2 = Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style3 = Tui.styled { fg = Just Ansi.Color.magenta, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style4 = Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style5 = Tui.styled { fg = Just Ansi.Color.blue, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style6 = Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [], hyperlink = Nothing }
    , style7 = Tui.styled { fg = Just Ansi.Color.brightCyan, bg = Nothing, attributes = [], hyperlink = Nothing }
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
        , Tui.styled { fg = Just Ansi.Color.white, bg = Just color, attributes = [ Tui.Bold ], hyperlink = Nothing }
            (" " ++ magnitude ++ " CHANGE ")
        ]


changeSummaryLine : ApiDiff -> Tui.Screen
changeSummaryLine diff =
    let
        addedCount =
            List.length diff.addedModules
                + (diff.changedModules |> List.map (\mc -> List.length mc.added) |> List.sum)

        changedCount =
            diff.changedModules |> List.map (\mc -> List.length mc.changed) |> List.sum

        removedCount =
            List.length diff.removedModules
                + (diff.changedModules |> List.map (\mc -> List.length mc.removed) |> List.sum)

        moduleCount =
            List.length diff.addedModules
                + List.length diff.removedModules
                + List.length diff.changedModules

        parts =
            (if addedCount > 0 then
                [ Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing }
                    ("+" ++ String.fromInt addedCount ++ " added")
                ]

             else
                []
            )
                ++ (if changedCount > 0 then
                        [ Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [], hyperlink = Nothing }
                            ("~" ++ String.fromInt changedCount ++ " changed")
                        ]

                    else
                        []
                   )
                ++ (if removedCount > 0 then
                        [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [], hyperlink = Nothing }
                            ("-" ++ String.fromInt removedCount ++ " removed")
                        ]

                    else
                        []
                   )

        separator =
            Tui.text "  "

        statsWithSeparators =
            parts
                |> List.intersperse separator

        moduleCountText =
            Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
                (" across " ++ String.fromInt moduleCount ++ " modules")
    in
    Tui.concat
        ([ Tui.text "  " ]
            ++ statsWithSeparators
            ++ [ moduleCountText ]
        )


renderModuleDiff : Int -> ApiDiff -> List Docs.Module -> String -> List Tui.Screen
renderModuleDiff wrapWidth diff modules moduleName =
    let
        isNewModule =
            List.member moduleName diff.addedModules

        isRemovedModule =
            List.member moduleName diff.removedModules

        maybeModule =
            findModule moduleName modules
    in
    if isNewModule then
        renderNewModule wrapWidth moduleName maybeModule

    else if isRemovedModule then
        renderRemovedModule moduleName

    else
        renderChangedModule wrapWidth diff moduleName maybeModule


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


renderNewModule : Int -> String -> Maybe Docs.Module -> List Tui.Screen
renderNewModule wrapWidth moduleName maybeModule =
    let
        header =
            [ Tui.concat
                [ Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.green, attributes = [ Tui.Bold ], hyperlink = Nothing } " + "
                , Tui.text " "
                , Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } moduleName
                ]
            , Tui.text ""
            ]
    in
    case maybeModule of
        Just mod ->
            header ++ renderModuleDocsWithGutter wrapWidth Ansi.Color.green mod

        Nothing ->
            header


renderRemovedModule : String -> List Tui.Screen
renderRemovedModule moduleName =
    [ Tui.concat
        [ Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.red, attributes = [ Tui.Bold ], hyperlink = Nothing } " - "
        , Tui.text " "
        , Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold, Tui.Strikethrough ], hyperlink = Nothing } moduleName
        ]
    , Tui.text ""
    ]


renderChangedModule : Int -> ApiDiff -> String -> Maybe Docs.Module -> List Tui.Screen
renderChangedModule wrapWidth diff moduleName maybeModule =
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
                        renderItemBlock wrapWidth diff moduleName itemName Diff.Added maybeModule
                    )

        changedSection =
            changedItems
                |> List.concatMap
                    (\itemName ->
                        renderItemBlock wrapWidth diff moduleName itemName Diff.Changed maybeModule
                    )

        removedSection =
            if List.isEmpty removedItems then
                []

            else
                Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } "  Removed:"
                    :: List.map
                        (\item ->
                            Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Strikethrough ], hyperlink = Nothing } ("    " ++ item)
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


renderItemBlock : Int -> ApiDiff -> String -> String -> Diff.DiffStatus -> Maybe Docs.Module -> List Tui.Screen
renderItemBlock wrapWidth diff moduleName itemName status maybeModule =
    let
        badge =
            case status of
                Diff.Added ->
                    Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.green, attributes = [ Tui.Bold ], hyperlink = Nothing } " + "

                Diff.Changed ->
                    Tui.styled { fg = Just Ansi.Color.white, bg = Just Ansi.Color.yellow, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "

                _ ->
                    Tui.text "   "

        oldTypeLine =
            case status of
                Diff.Changed ->
                    renderOldType diff moduleName itemName

                _ ->
                    []

        gutterColor =
            case status of
                Diff.Added ->
                    Ansi.Color.green

                Diff.Changed ->
                    Ansi.Color.yellow

                _ ->
                    Ansi.Color.brightBlack

        blockLines =
            case maybeModule of
                Just mod ->
                    case findBlock itemName mod of
                        Just block ->
                            renderBlock wrapWidth gutterColor block

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
                    [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Strikethrough ], hyperlink = Nothing }
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
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } "  README Changes"
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
                Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } "  Doc changes:"
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
                            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } ("    " ++ label ++ ":")
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
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [], hyperlink = Nothing } ("    " ++ line)

                else if String.startsWith "- " line || String.startsWith "-" line then
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [], hyperlink = Nothing } ("    " ++ line)

                else
                    Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing } ("    " ++ line)
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
            ++ (if Toast.hasToasts model.toasts then
                    [ Tui.Sub.every 100 ToastTick ]

                else
                    []
               )
        )
