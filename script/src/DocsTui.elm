module DocsTui exposing (InitData, Model, Msg, appConfig)

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
import SyntaxHighlight
import Tui
import Tui.Effect as Effect
import Tui.Layout as Layout


type alias InitData =
    { modules : List Docs.Module
    , diff : Maybe ApiDiff
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    , dependencies : List String
    , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
    , readme : Maybe String
    , packageVersion : Maybe String
    }


type alias Model =
    { modules : List Docs.Module
    , diff : Maybe ApiDiff
    , rightView : RightView
    , activeLeftTab : LeftTab
    , versions : List String
    , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
    , selectedModuleName : String
    , loadingDiff : Bool
    , diffVersion : Maybe String
    , cachedDocsContent : Maybe { key : String, content : List Tui.Screen }
    , cachedRightPane : Maybe { key : String, title : String, lines : List Tui.Screen }
    , itemLinePositions : List ( String, Int )
    , internalLinks : List { line : Int, destination : String, text : String }
    , preRenderedModules : Dict.Dict String { lines : List Tui.Screen, items : List String, itemPositions : List ( String, Int ) }
    , selectedItemIndex : Int
    , dependencies : List String
    , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
    , readme : Maybe String
    , modal : Maybe ModalKind
    , browsingPackage : Maybe String
    , homeModules : List Docs.Module
    , packageVersion : Maybe String
    }


type ModalKind
    = HelpView
    | PackagePickerModal
    | ComparePickerModal String


type RightView
    = DocsView
    | DiffView


type LeftTab
    = ModulesTab
    | ChangesTab
    | VersionsTab


type Msg
    = SelectModule String
    | SelectItemByName String
    | CloseModal
    | BrowsePackage String
    | CompareVersion String
    | RawEvent Layout.RawEvent
    | DocsPaneScrolled Int
    | OpenHelp
    | ToggleDiffView
    | SwitchToModulesTab
    | SwitchToChangesTab
    | SwitchToVersionsTab
    | Quit
    | GotDiff (Maybe ApiDiff)
    | GotPackageDocs String (Result FatalError (List Docs.Module))
    | OpenPackagePicker


appConfig :
    { init : InitData -> ( Model, Effect.Effect Msg )
    , update : Layout.UpdateContext -> Msg -> Model -> ( Model, Effect.Effect Msg )
    , view : Tui.Context -> Model -> Layout.Layout Msg
    , bindings : { focusedPane : Maybe String } -> Model -> List (Layout.Group Msg)
    , status : Model -> { waiting : Maybe String }
    , modal : Model -> Maybe (Layout.Modal Msg)
    , onRawEvent : Maybe (Layout.RawEvent -> Msg)
    }
appConfig =
    { init = init
    , update = update
    , view = viewLayout
    , bindings = bindings
    , status = statusConfig
    , modal = modalConfig
    , onRawEvent = Just RawEvent
    }


statusConfig : Model -> { waiting : Maybe String }
statusConfig model =
    { waiting =
        if model.loadingDiff then
            Just "Loading..."

        else
            Nothing
    }


modalConfig : Model -> Maybe (Layout.Modal Msg)
modalConfig model =
    case model.modal of
        Just HelpView ->
            Just (Layout.helpModal CloseModal)

        Just PackagePickerModal ->
            Just
                (Layout.pickerModal
                    { items = model.dependencies
                    , toString = identity
                    , title = "Browse Package"
                    , onSelect = \pkg -> BrowsePackage pkg
                    , onCancel = CloseModal
                    }
                )

        Just (ComparePickerModal baseVersion) ->
            Just
                (Layout.pickerModal
                    { items = "HEAD" :: List.filter (\v -> v /= baseVersion) model.versions
                    , toString = identity
                    , title = "Compare " ++ baseVersion ++ " against"
                    , onSelect = \v -> CompareVersion v
                    , onCancel = CloseModal
                    }
                )

        Nothing ->
            Nothing


init : InitData -> ( Model, Effect.Effect Msg )
init { modules, diff, versions, loadDiff, dependencies, loadPackageDocs, readme, packageVersion } =
    ( { modules = modules
      , diff = diff
      , rightView = DocsView
      , activeLeftTab = ModulesTab
      , versions = versions
      , loadDiff = loadDiff
      , selectedModuleName =
            modules
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault ""
      , loadingDiff = False
      , diffVersion = Nothing
      , cachedDocsContent = Nothing
      , cachedRightPane = Nothing
      , itemLinePositions = []
      , internalLinks = []
      , preRenderedModules = Dict.empty
      , selectedItemIndex = 0
      , dependencies = dependencies
      , loadPackageDocs = loadPackageDocs
      , readme = readme
      , modal = Nothing
      , browsingPackage = Nothing
      , homeModules = modules
      , packageVersion = packageVersion
      }
    , Effect.none
    )


visibleEntries : Model -> List String
visibleEntries model =
    case model.activeLeftTab of
        ModulesTab ->
            let
                readmeEntry =
                    case model.readme of
                        Just _ ->
                            [ "README" ]

                        Nothing ->
                            []
            in
            readmeEntry ++ List.map .name model.modules

        ChangesTab ->
            case model.diff of
                Just diff ->
                    changedModuleNames diff

                Nothing ->
                    List.map .name model.modules

        VersionsTab ->
            model.versions


changedModuleNames : ApiDiff -> List String
changedModuleNames diff =
    let
        readmeEntry =
            case diff.readmeDiff of
                Just _ ->
                    [ "README" ]

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
    in
    readmeEntry ++ allChangedNames


selectedEntryName : Layout.UpdateContext -> Model -> Maybe String
selectedEntryName ctx model =
    if String.isEmpty model.selectedModuleName then
        Layout.selectedItem "modules" (visibleEntries model) (viewLayout ctx.context model) (layoutStateFromCtx ctx)

    else
        Just model.selectedModuleName


{-| Build a minimal Layout.State from UpdateContext for selectedItem lookups.
-}
layoutStateFromCtx : Layout.UpdateContext -> Layout.State
layoutStateFromCtx ctx =
    Layout.init
        |> Layout.withContext ctx.context


selectedEntryNameFromModel : Model -> Maybe String
selectedEntryNameFromModel model =
    if String.isEmpty model.selectedModuleName then
        Nothing

    else
        Just model.selectedModuleName


selectedModule : Model -> Maybe Docs.Module
selectedModule model =
    case selectedEntryNameFromModel model of
        Just name ->
            findModule name model.modules

        Nothing ->
            Nothing


bindings : { focusedPane : Maybe String } -> Model -> List (Layout.Group Msg)
bindings { focusedPane } model =
    let
        hasDiff =
            model.diff /= Nothing

        hasVersions =
            not (List.isEmpty model.versions)

        diffToggleBinding =
            [ Layout.charBinding 'd' "Toggle diff view" ToggleDiffView ]

        changesTabBinding =
            if hasDiff || hasVersions then
                [ Layout.charBinding 'c' "Changes" SwitchToChangesTab ]

            else
                []

        versionsTabBinding =
            if hasVersions then
                [ Layout.charBinding 'v' "Versions" SwitchToVersionsTab ]

            else
                []

        packagePickerBinding =
            if not (List.isEmpty model.dependencies) then
                [ Layout.charBinding 'p' "Browse package" OpenPackagePicker ]

            else
                []

        versionEnterBinding =
            []
    in
    [ Layout.group "Global"
        ([ Layout.charBinding 'q' "Quit" Quit
         , Layout.charBinding '?' "Toggle help" OpenHelp
         , Layout.charBinding 'h' "Focus left" (focusLeftMsg focusedPane)
         , Layout.charBinding 'l' "Focus right" (focusRightMsg focusedPane)
         ]
            ++ diffToggleBinding
            ++ changesTabBinding
            ++ versionsTabBinding
            ++ packagePickerBinding
            ++ versionEnterBinding
        )
    ]




focusLeftMsg : Maybe String -> Msg
focusLeftMsg _ =
    -- This is handled by the RawEvent path for now
    -- But bindings produce direct messages, so we use a placeholder
    RawEvent (Layout.UnhandledKey { key = Tui.Character 'h', modifiers = [] })


focusRightMsg : Maybe String -> Msg
focusRightMsg _ =
    RawEvent (Layout.UnhandledKey { key = Tui.Character 'l', modifiers = [] })


update : Layout.UpdateContext -> Msg -> Model -> ( Model, Effect.Effect Msg )
update ctx msg model =
    case msg of
        SelectModule name ->
            ( { model
                | selectedModuleName = name
                , cachedDocsContent = Nothing
                , cachedRightPane = Nothing
                , selectedItemIndex = 0
              }
                |> refreshRightPaneCache ctx.context
            , Effect.batch [ Effect.resetScroll "docs", Effect.setSelectedIndex "items" 0 ]
            )

        SelectItemByName name ->
            let
                scrollTarget =
                    model.itemLinePositions
                        |> List.filter (\( itemName, _ ) -> itemName == name)
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 0

                items =
                    itemsForCurrentView model

                newIdx =
                    items
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, item ) -> item == name)
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault model.selectedItemIndex
            in
            ( { model | selectedItemIndex = newIdx }
            , Effect.batch [ Effect.resetScroll "docs", Effect.scrollDown "docs" (max 0 (scrollTarget - 2)) ]
            )

        CloseModal ->
            ( { model | modal = Nothing }, Effect.none )

        BrowsePackage packageName ->
            ( { model
                | modal = Nothing
                , loadingDiff = True
              }
            , Effect.batch
                [ Effect.toast ("Loading " ++ packageName ++ "...")
                , Effect.attempt (GotPackageDocs packageName) (model.loadPackageDocs packageName)
                ]
            )

        CompareVersion targetVersion ->
            case model.modal of
                Just (ComparePickerModal baseVersion) ->
                    let
                        ( olderVersion, newerVersion ) =
                            orderVersions baseVersion targetVersion

                        diffLabel =
                            olderVersion ++ " -> " ++ newerVersion
                    in
                    ( { model
                        | modal = Nothing
                        , loadingDiff = True
                        , diffVersion = Just diffLabel
                      }
                    , Effect.batch
                        [ Effect.toast ("Loading diff " ++ diffLabel ++ "...")
                        , Effect.perform GotDiff (model.loadDiff olderVersion)
                        ]
                    )

                _ ->
                    ( { model | modal = Nothing }, Effect.none )

        RawEvent rawEvent ->
            handleRawEvent ctx rawEvent model

        DocsPaneScrolled scrollOffset ->
            let
                newModel =
                    syncItemsToScrollAt scrollOffset model
            in
            ( newModel
            , Effect.setSelectedIndex "items" newModel.selectedItemIndex
            )

        OpenHelp ->
            case model.modal of
                Just HelpView ->
                    ( { model | modal = Nothing }, Effect.none )

                _ ->
                    ( { model | modal = Just HelpView }, Effect.none )

        ToggleDiffView ->
            handleToggleDiffView ctx model

        SwitchToModulesTab ->
            let
                modulesModel =
                    { model | activeLeftTab = ModulesTab, rightView = DocsView, cachedRightPane = Nothing }

                firstEntry =
                    visibleEntries modulesModel
                        |> List.head
                        |> Maybe.withDefault ""
            in
            ( { modulesModel | selectedModuleName = firstEntry }
                |> refreshRightPaneCache ctx.context
            , Effect.none
            )

        SwitchToChangesTab ->
            case model.diff of
                Just _ ->
                    let
                        changesModel =
                            { model | activeLeftTab = ChangesTab, rightView = DiffView, cachedRightPane = Nothing }

                        firstEntry =
                            visibleEntries changesModel
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    ( { changesModel | selectedModuleName = firstEntry }
                        |> refreshRightPaneCache ctx.context
                    , Effect.none
                    )

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

        SwitchToVersionsTab ->
            if List.isEmpty model.versions then
                ( model, Effect.none )

            else
                let
                    firstVersion =
                        model.versions |> List.head |> Maybe.withDefault ""
                in
                ( { model | activeLeftTab = VersionsTab, selectedModuleName = firstVersion }, Effect.none )

        Quit ->
            ( model, Effect.exit )

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
                , cachedDocsContent = Nothing
                , cachedRightPane = Nothing
              }
                |> refreshRightPaneCache ctx.context
            , Effect.toast toastMessage
            )

        GotPackageDocs packageName result ->
            case result of
                Ok newModules ->
                    ( { model
                        | modules = newModules
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
                                (docsPaneWidth ctx.context - 2)
                                newModules
                      }
                        |> refreshRightPaneCache ctx.context
                    , Effect.toast ("Browsing " ++ packageName)
                    )

                Err _ ->
                    ( { model | loadingDiff = False }
                    , Effect.errorToast ("Failed to load " ++ packageName)
                    )

        OpenPackagePicker ->
            if List.isEmpty model.dependencies then
                ( model, Effect.none )

            else
                ( { model | modal = Just PackagePickerModal }
                , Effect.none
                )


handleToggleDiffView : Layout.UpdateContext -> Model -> ( Model, Effect.Effect Msg )
handleToggleDiffView ctx model =
    if model.loadingDiff then
        ( model, Effect.none )

    else if model.activeLeftTab == VersionsTab then
        -- On versions tab: open compare-against picker
        case selectedEntryName ctx model of
            Just name ->
                ( { model | modal = Just (ComparePickerModal name) }
                , Effect.none
                )

            _ ->
                -- Also handle as version loading when on versions tab
                case selectedEntryNameFromModel model of
                    Just name ->
                        ( { model | loadingDiff = True, diffVersion = Just name }
                        , Effect.perform GotDiff (model.loadDiff name)
                        )

                    Nothing ->
                        ( model, Effect.none )

    else
        case model.diff of
            Just _ ->
                let
                    ( newView, newTab ) =
                        case model.rightView of
                            DocsView ->
                                ( DiffView, ChangesTab )

                            DiffView ->
                                ( DocsView, ModulesTab )
                in
                ( { model
                    | rightView = newView
                    , activeLeftTab = newTab
                    , cachedRightPane = Nothing
                  }
                    |> refreshRightPaneCache ctx.context
                , Effect.none
                )

            Nothing ->
                case model.versions of
                    firstVersion :: _ ->
                        ( { model | loadingDiff = True, diffVersion = Just firstVersion }
                        , Effect.perform GotDiff (model.loadDiff firstVersion)
                        )

                    [] ->
                        -- No versions from git tags — diff against published version
                        case model.packageVersion of
                            Just version ->
                                ( { model | loadingDiff = True, diffVersion = Just version }
                                , Effect.perform GotDiff (model.loadDiff version)
                                )

                            Nothing ->
                                ( model, Effect.none )


handleRawEvent : Layout.UpdateContext -> Layout.RawEvent -> Model -> ( Model, Effect.Effect Msg )
handleRawEvent ctx rawEvent model =
    case rawEvent of
        Layout.UnhandledKey keyEvent ->
            case keyEvent.key of
                Tui.Character 'h' ->
                    let
                        newPane =
                            case ctx.focusedPane of
                                Just "docs" ->
                                    "items"

                                Just "items" ->
                                    "modules"

                                _ ->
                                    "modules"
                    in
                    ( model, Effect.focusPane newPane )

                Tui.Character 'l' ->
                    let
                        newPane =
                            case ctx.focusedPane of
                                Just "modules" ->
                                    "items"

                                Just "items" ->
                                    "docs"

                                _ ->
                                    "docs"
                    in
                    ( model, Effect.focusPane newPane )

                Tui.Escape ->
                    ( model, Effect.exit )

                Tui.Enter ->
                    -- Handle Enter for various contexts
                    case ctx.focusedPane of
                        Just "modules" ->
                            if model.activeLeftTab == VersionsTab then
                                -- Load diff for selected version
                                if model.loadingDiff then
                                    ( model, Effect.none )

                                else
                                    case selectedEntryName ctx model of
                                        Just name ->
                                            ( { model | loadingDiff = True, diffVersion = Just name }
                                            , Effect.perform GotDiff (model.loadDiff name)
                                            )

                                        _ ->
                                            ( model, Effect.none )

                            else
                                ( model, Effect.focusPane "docs" )

                        Just "items" ->
                            -- Jump to selected item in docs
                            let
                                items =
                                    itemsForCurrentView model

                                selectedIdx =
                                    ctx.selectedIndex "items"

                                maybeItemName =
                                    items |> List.drop selectedIdx |> List.head

                                docsContent =
                                    currentDocsContent ctx model

                                scrollTarget =
                                    case maybeItemName of
                                        Just itemName ->
                                            findLineForItem itemName docsContent

                                        Nothing ->
                                            0
                            in
                            ( model
                            , Effect.batch [ Effect.resetScroll "docs", Effect.scrollDown "docs" (max 0 (scrollTarget - 2)) ]
                            )

                        Just "docs" ->
                            -- Follow link
                            handleFollowLink ctx model

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        Layout.Click { row, col } ->
            -- Handle clicks on internal links in the docs pane
            case handleDocsPaneClick ctx row model of
                Just result ->
                    result

                Nothing ->
                    ( model, Effect.none )

        Layout.Scroll _ ->
            ( model, Effect.none )


handleFollowLink : Layout.UpdateContext -> Model -> ( Model, Effect.Effect Msg )
handleFollowLink ctx model =
    let
        scrollOffset =
            ctx.scrollPosition "docs"

        visibleTop =
            scrollOffset

        visibleBottom =
            scrollOffset + ctx.context.height - 4

        links =
            case selectedModule model of
                Just mod ->
                    let
                        rawLinks =
                            extractLinksFromComment mod.comment

                        docsContent =
                            currentDocsContent ctx model
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
            navigateToLink ctx link.destination model

        Nothing ->
            ( model, Effect.none )


handleDocsPaneClick : Layout.UpdateContext -> Int -> Model -> Maybe ( Model, Effect.Effect Msg )
handleDocsPaneClick ctx row model =
    let
        scrollOffset =
            ctx.scrollPosition "docs"

        contentLine =
            row - 1 + scrollOffset

        clickedLineText =
            case model.cachedRightPane of
                Just cached ->
                    cached.lines
                        |> List.drop contentLine
                        |> List.head
                        |> Maybe.map Tui.toString
                        |> Maybe.withDefault ""

                Nothing ->
                    ""

        links =
            case selectedModule model of
                Just mod ->
                    extractLinksFromComment mod.comment
                        ++ List.concatMap (.comment >> extractLinksFromComment) mod.values
                        ++ List.concatMap (.comment >> extractLinksFromComment) mod.unions
                        ++ List.concatMap (.comment >> extractLinksFromComment) mod.aliases

                Nothing ->
                    []

        matchingLink =
            links
                |> List.filter (\link -> String.contains link.text clickedLineText)
                |> List.head
    in
    if row > 0 && not (String.isEmpty clickedLineText) then
        case matchingLink of
            Just link ->
                Just (navigateToLink ctx link.destination model)

            Nothing ->
                Nothing

    else
        Nothing


navigateToLink : Layout.UpdateContext -> String -> Model -> ( Model, Effect.Effect Msg )
navigateToLink ctx destination model =
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
                        |> List.filter (\( _, entry ) -> entry == moduleName)
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            case maybeIdx of
                Just idx ->
                    let
                        newModel =
                            { model
                                | selectedModuleName = moduleName
                                , cachedDocsContent = Nothing
                                , cachedRightPane = Nothing
                            }
                                |> refreshRightPaneCache ctx.context

                        scrollEffect =
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
                                    Effect.batch
                                        [ Effect.setSelectedIndex "modules" idx
                                        , Effect.resetScroll "docs"
                                        , Effect.scrollDown "docs" (max 0 (scrollTarget - 2))
                                        , Effect.setSelectedIndex "items" 0
                                        , Effect.toast ("-> " ++ moduleName)
                                        ]

                                Nothing ->
                                    Effect.batch
                                        [ Effect.setSelectedIndex "modules" idx
                                        , Effect.resetScroll "docs"
                                        , Effect.setSelectedIndex "items" 0
                                        , Effect.toast ("-> " ++ moduleName)
                                        ]
                    in
                    ( newModel, scrollEffect )

                Nothing ->
                    ( model
                    , Effect.errorToast ("Module " ++ moduleName ++ " not found")
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
                    ( model
                    , Effect.batch [ Effect.resetScroll "docs", Effect.scrollDown "docs" (max 0 (scrollTarget - 2)) ]
                    )

                Nothing ->
                    ( model, Effect.none )



-- VIEW


viewLayout : Tui.Context -> Model -> Layout.Layout Msg
viewLayout ctx model =
    Layout.horizontal
        [ modulesPane ctx model
        , itemsPane ctx model
        , rightPane ctx model
        ]


modulesPaneWidth : Int -> Layout.Width
modulesPaneWidth termWidth =
    Layout.fixed (min 28 (termWidth // 4))


modulesPane : Tui.Context -> Model -> Layout.Pane Msg
modulesPane ctx model =
    let
        entries =
            visibleEntries model

        entryCount =
            List.length entries

        showBadges =
            (model.rightView == DiffView || model.activeLeftTab == ChangesTab)
                && model.diff /= Nothing

        base =
            Layout.selectableList
                { onSelect = \entry -> SelectModule entry
                , view = \{ selection } entry -> renderModuleEntry selection showBadges model entry
                }
                entries

        withTree =
            case model.activeLeftTab of
                VersionsTab ->
                    base

                _ ->
                    base
                        |> Layout.withTreeView { toPath = String.split "." } entries

        selectedPos =
            entries
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, e ) -> e == model.selectedModuleName)
                |> List.head
                |> Maybe.map (\( i, _ ) -> i + 1)
                |> Maybe.withDefault 1
    in
    Layout.pane "modules"
        { title = "Modules"
        , width = modulesPaneWidth ctx.width
        }
        (withTree
            |> Layout.withFilterable identity entries
        )
        |> Layout.withInlineFooter
            (Tui.text
                (String.fromInt selectedPos
                    ++ " of "
                    ++ String.fromInt entryCount
                )
                |> Tui.dim
            )


renderModuleEntry : Layout.SelectionState -> Bool -> Model -> String -> Tui.Screen
renderModuleEntry selection showBadges model name =
    let
        badge =
            if showBadges then
                moduleBadge model name

            else
                Tui.empty
    in
    case selection of
        Layout.Selected { focused } ->
            Tui.concat
                [ badge
                , Tui.styled
                    { fg = Just Ansi.Color.white
                    , bg = Just Ansi.Color.blue
                    , attributes =
                        if focused then
                            [ Tui.Bold ]

                        else
                            []
                    , hyperlink = Nothing
                    }
                    (name ++ " ")
                ]

        Layout.NotSelected ->
            Tui.concat
                [ badge
                , Tui.text name
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


topEntry : String
topEntry =
    "⬆ Top"


itemsForCurrentView : Model -> List String
itemsForCurrentView model =
    let
        name =
            model.selectedModuleName

        withTop items =
            if List.isEmpty items then
                []

            else
                topEntry :: items
    in
    if String.isEmpty name then
        []

    else
        case model.rightView of
            DiffView ->
                case model.diff of
                    Just diff ->
                        if List.member name diff.addedModules then
                            case findModule name model.modules of
                                Just mod ->
                                    withTop (moduleItemNames mod)

                                Nothing ->
                                    []

                        else
                            withTop (diffItemNames diff name)

                    Nothing ->
                        []

            DocsView ->
                if name == "README" then
                    case model.readme of
                        Just readmeContent ->
                            withTop (extractHeadings readmeContent)

                        Nothing ->
                            []

                else
                    case findModule name model.modules of
                        Just mod ->
                            withTop (moduleItemNames mod)

                        Nothing ->
                            []


diffItemNames : ApiDiff -> String -> List String
diffItemNames diff moduleName =
    case Diff.findModuleChanges moduleName diff.changedModules of
        Just changes ->
            changes.added ++ changes.changed ++ changes.removed

        Nothing ->
            if List.member moduleName diff.addedModules then
                []

            else
                []


itemsPane : Tui.Context -> Model -> Layout.Pane Msg
itemsPane _ model =
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
            { onSelect = \item -> SelectItemByName item
            , view = \{ selection } item -> renderItemEntry selection item
            }
            items
        )
        |> Layout.withInlineFooter
            (if itemCount > 0 then
                Tui.text
                    (String.fromInt (model.selectedItemIndex + 1)
                        ++ " of "
                        ++ String.fromInt itemCount
                    )
                    |> Tui.dim

             else
                Tui.empty
            )


renderItemEntry : Layout.SelectionState -> String -> Tui.Screen
renderItemEntry selection name =
    case selection of
        Layout.Selected { focused } ->
            Tui.styled
                { fg = Just Ansi.Color.white
                , bg = Just Ansi.Color.blue
                , attributes =
                    if focused then
                        [ Tui.Bold ]

                    else
                        []
                , hyperlink = Nothing
                }
                (name ++ " ")

        Layout.NotSelected ->
            if String.startsWith "# " name then
                Tui.text name |> Tui.dim |> Tui.bold

            else
                Tui.text name


syncItemsToScrollAt : Int -> Model -> Model
syncItemsToScrollAt scrollOffset model =
    if List.isEmpty model.itemLinePositions then
        model

    else if scrollOffset == 0 then
        -- At the very top → select "⬆ Top"
        { model | selectedItemIndex = 0 }

    else
        let
            anchor =
                scrollOffset + 3

            -- Find which item position we've scrolled past
            rawIndex =
                model.itemLinePositions
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( idx, ( _, linePos ) ) best ->
                            if linePos <= anchor then
                                idx

                            else
                                best
                        )
                        -1

            -- +1 to account for "⬆ Top" at index 0
            -- rawIndex -1 means we haven't reached the first item → select Top (index 0)
            bestIndex =
                rawIndex + 1
        in
        { model | selectedItemIndex = bestIndex }


rightPaneCacheKey : Int -> Model -> String
rightPaneCacheKey wrapWidth model =
    docsContentCacheKey model ++ ":" ++ String.fromInt wrapWidth


extractLinksFromComment : String -> List { destination : String, text : String }
extractLinksFromComment comment =
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
                            Just { destination = destination, text = String.replace "`" "" linkText }

                    _ ->
                        Nothing
            )


findLinkLine : String -> List Tui.Screen -> Int
findLinkLine linkText lines =
    lines
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, line ) -> String.contains linkText (Tui.toString line))
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1


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


refreshRightPaneCache : Tui.Context -> Model -> Model
refreshRightPaneCache ctx model =
    let
        wrapWidth =
            docsPaneWidth ctx - 2

        key =
            rightPaneCacheKey wrapWidth model

        buildAndCachePositions m =
            let
                selectedName =
                    case selectedEntryNameFromModel m of
                        Just name ->
                            name

                        _ ->
                            ""

                preRendered =
                    Dict.get selectedName m.preRenderedModules
            in
            case preRendered of
                Just rendered ->
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
                    let
                        fallbackName =
                            case selectedEntryNameFromModel m of
                                Just name ->
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
            case selectedEntryNameFromModel model of
                Just name ->
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
                                    case selectedEntryNameFromModel model of
                                        Just name ->
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
        ( b, "HEAD" )

    else if b == "HEAD" then
        ( a, "HEAD" )

    else
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
            case selectedEntryNameFromModel model of
                Just name ->
                    name

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


currentDocsContent : Layout.UpdateContext -> Model -> List Tui.Screen
currentDocsContent ctx model =
    let
        key =
            docsContentCacheKey model
    in
    case model.cachedDocsContent of
        Just cached ->
            if cached.key == key then
                cached.content

            else
                buildDocsContent ctx model

        Nothing ->
            buildDocsContent ctx model


buildDocsContent : Layout.UpdateContext -> Model -> List Tui.Screen
buildDocsContent ctx model =
    let
        width =
            ctx.context.width
    in
    case model.rightView of
        DocsView ->
            case selectedModule model of
                Just mod ->
                    renderModuleDocs width mod

                Nothing ->
                    []

        DiffView ->
            case ( model.diff, selectedEntryNameFromModel model ) of
                ( Just diff, Just name ) ->
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
        indexedTextLines =
            lines
                |> List.indexedMap (\idx line -> ( idx, Tui.toString line ))

        findItem itemName =
            let
                headingText =
                    String.dropLeft 2 itemName

                matchLine ( _, text ) =
                    if String.startsWith "# " itemName then
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
    List.map findItem items


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
                    String.contains (String.dropLeft 2 itemName) text
                        && String.contains "##" text

                else
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


stripMarkdownInline : String -> String
stripMarkdownInline text =
    text
        |> stripLinks
        |> String.replace "`" ""
        |> String.replace "**" ""
        |> String.replace "__" ""


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


rightPane : Tui.Context -> Model -> Layout.Pane Msg
rightPane ctx model =
    if model.loadingDiff then
        Layout.pane "docs"
            { title = "Loading..."
            , width = Layout.fill
            }
            (Layout.content
                [ Tui.text "Loading diff..." |> Tui.dim
                ]
            )

    else
        let
            wrapWidth =
                docsPaneWidth ctx - 2

            cached =
                case model.cachedRightPane of
                    Just c ->
                        c

                    Nothing ->
                        { key = "", title = "Docs", lines = [ Tui.text "Loading..." ] }
        in
        Layout.pane "docs"
            { title = cached.title
            , width = Layout.fill
            }
            (Layout.content cached.lines
                |> Layout.withSearchable
            )
            |> Layout.withOnScroll DocsPaneScrolled



-- DOCS RENDERING


renderModuleDocs : Int -> Docs.Module -> List Tui.Screen
renderModuleDocs wrapWidth mod =
    renderModuleDocsWithGutter wrapWidth Ansi.Color.brightBlack mod


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
                                    let
                                        headings =
                                            extractHeadings markdown
                                    in
                                    case headings of
                                        [] ->
                                            acc.positions

                                        _ ->
                                            List.foldl
                                                (\heading posAcc ->
                                                    let
                                                        headingText =
                                                            String.dropLeft 2 heading

                                                        lineOffset =
                                                            blockLines
                                                                |> List.indexedMap Tuple.pair
                                                                |> List.filter
                                                                    (\( _, line ) ->
                                                                        let
                                                                            text =
                                                                                Tui.toString line
                                                                        in
                                                                        String.contains headingText text
                                                                            && String.contains "##" text
                                                                    )
                                                                |> List.head
                                                                |> Maybe.map Tuple.first
                                                                |> Maybe.withDefault 0
                                                    in
                                                    posAcc ++ [ ( heading, acc.lineCount + lineOffset ) ]
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
        Tui.text ""
            :: (trimmed
                    |> String.lines
                    |> List.map Tui.text
                    |> wrapLines commentWidth
               )
            |> List.map (indentScreen "    ")


indentScreen : String -> Tui.Screen -> Tui.Screen
indentScreen prefix screen =
    Tui.concat [ Tui.text prefix, screen ]


wrapLines : Int -> List Tui.Screen -> List Tui.Screen
wrapLines maxWidth lines =
    if maxWidth <= 0 then
        lines

    else
        List.concatMap (Tui.wrapWidth maxWidth) lines



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
            markdown
                |> String.lines
                |> List.map Tui.text


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


inlineChildrenToString : List (List Tui.Screen) -> String
inlineChildrenToString children =
    children
        |> List.concat
        |> List.map Tui.toString
        |> String.join ""


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
