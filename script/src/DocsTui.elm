module DocsTui exposing (Model, Msg, init, subscriptions, update, view)

{-| Unified TUI for browsing Elm documentation and viewing API diffs.

Supports two modes:

  - **Browse mode**: No diff data, shows all modules with full docs in the right pane.
  - **Diff mode**: With diff data, `d` toggles between docs view and diff view.

Left pane: list of modules (with diff badges when in diff view).
Right pane: full docs for the selected module, or diff details when toggled.

-}

import Ansi.Color
import Dict
import Docs.Diff as Diff exposing (ApiDiff, ModuleChanges)
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import Json.Decode as Decode
import ModuleTree exposing (TreeEntry(..))
import Tui
import Tui.Effect as Effect
import Tui.Input as Input
import Tui.Keybinding as Keybinding
import Tui.Layout as Layout
import Tui.Modal as Modal
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
    }


type RightView
    = DocsView
    | DiffView


type LeftTab
    = ModulesTab
    | ChangesTab


type Msg
    = KeyPressed Tui.KeyEvent
    | MouseEvent Tui.MouseEvent
    | SelectEntry Int
    | GotContext { width : Int, height : Int }


init : { modules : List Docs.Module, diff : Maybe ApiDiff } -> ( Model, Effect.Effect Msg )
init { modules, diff } =
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
                            String.contains query (String.toLower (ModuleTree.entryName entry))
                        )

        Nothing ->
            baseEntries


changedModuleEntries : ApiDiff -> List TreeEntry
changedModuleEntries diff =
    let
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
    allChangedNames
        |> List.map (\name -> Leaf { name = name, depth = 0 })


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

        GotContext ctx ->
            ( { model
                | layout =
                    Layout.withContext ctx model.layout
              }
            , Effect.none
            )


type Action
    = Quit
    | NavigateDown
    | NavigateUp
    | FocusModules
    | FocusDocs
    | ScrollDocsDown
    | ScrollDocsUp
    | ToggleHelp
    | CloseHelp
    | ToggleDiffView
    | ToggleTreeNode
    | SwitchToModulesTab
    | SwitchToChangesTab
    | CycleLeftTab
    | ActivateFilter
    | CancelFilter


handleAction : Action -> Model -> ( Model, Effect.Effect Msg )
handleAction action model =
    case action of
        Quit ->
            ( model, Effect.exit )

        NavigateDown ->
            ( { model | layout = Layout.navigateDown "modules" model.layout }
            , Effect.none
            )

        NavigateUp ->
            ( { model | layout = Layout.navigateUp "modules" model.layout }
            , Effect.none
            )

        FocusModules ->
            ( { model | layout = Layout.focusPane "modules" model.layout }
            , Effect.none
            )

        FocusDocs ->
            ( { model | layout = Layout.focusPane "docs" model.layout }
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
            ( { model | activeLeftTab = ModulesTab }, Effect.none )

        SwitchToChangesTab ->
            case model.diff of
                Just _ ->
                    ( { model | activeLeftTab = ChangesTab }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        CycleLeftTab ->
            case model.diff of
                Just _ ->
                    let
                        nextTab =
                            case model.activeLeftTab of
                                ModulesTab ->
                                    ChangesTab

                                ChangesTab ->
                                    ModulesTab
                    in
                    ( { model | activeLeftTab = nextTab }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        ActivateFilter ->
            ( { model | filterInput = Just (Input.init "") }, Effect.none )

        CancelFilter ->
            ( { model | filterInput = Nothing }, Effect.none )



-- KEYBINDINGS


globalBindings : Model -> Keybinding.Group Action
globalBindings model =
    let
        diffBindings =
            case model.diff of
                Just _ ->
                    [ Keybinding.binding (Tui.Character 'd') "Toggle diff view" ToggleDiffView
                    , Keybinding.binding (Tui.Character '2') "Changes tab" SwitchToChangesTab
                    , Keybinding.binding Tui.Tab "Cycle tabs" CycleLeftTab
                    ]

                Nothing ->
                    []
    in
    Keybinding.group "Global"
        ([ Keybinding.binding (Tui.Character 'q') "Quit" Quit
         , Keybinding.binding Tui.Escape "Quit" Quit
         , Keybinding.binding (Tui.Character '?') "Toggle help" ToggleHelp
         , Keybinding.binding (Tui.Character 'h') "Focus modules" FocusModules
             |> Keybinding.withAlternate (Tui.Arrow Tui.Left)
         , Keybinding.binding (Tui.Character 'l') "Focus docs" FocusDocs
             |> Keybinding.withAlternate (Tui.Arrow Tui.Right)
         , Keybinding.binding (Tui.Character '1') "Modules tab" SwitchToModulesTab
         ]
            ++ diffBindings
        )


helpBindings : Keybinding.Group Action
helpBindings =
    Keybinding.group "Help"
        [ Keybinding.binding (Tui.Character '?') "Close help" ToggleHelp
        , Keybinding.binding Tui.Escape "Close help" CloseHelp
        , Keybinding.binding (Tui.Character 'q') "Quit" Quit
        ]


modulesBindings : Keybinding.Group Action
modulesBindings =
    Keybinding.group "Modules"
        [ Keybinding.binding (Tui.Character 'j') "Next module" NavigateDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Previous module" NavigateUp
            |> Keybinding.withAlternate (Tui.Arrow Tui.Up)
        , Keybinding.binding Tui.Enter "Toggle group" ToggleTreeNode
        , Keybinding.binding (Tui.Character '/') "Filter" ActivateFilter
        ]


docsBindings : Keybinding.Group Action
docsBindings =
    Keybinding.group "Docs"
        [ Keybinding.binding (Tui.Character 'j') "Scroll down" ScrollDocsDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Scroll up" ScrollDocsUp
            |> Keybinding.withAlternate (Tui.Arrow Tui.Up)
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

                    _ ->
                        [ modulesBindings ]
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
                Keybinding.helpRows "" [ modulesBindings, docsBindings, globalBindings model ]
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

                        diffHints =
                            case model.diff of
                                Just _ ->
                                    [ "d diff", "1/2 tabs" ]

                                Nothing ->
                                    []
                    in
                    base ++ diffHints

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
    Layout.px (min 40 (termWidth // 3))


leftPaneTitle : Model -> String
leftPaneTitle model =
    case model.diff of
        Just _ ->
            case model.activeLeftTab of
                ModulesTab ->
                    "[1]Modules [2]Changes"

                ChangesTab ->
                    "[1]Modules [2]Changes"

        Nothing ->
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
            { fg = Nothing
            , bg = Nothing
            , attributes = [ Tui.Bold, Tui.Inverse ]
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
                                    renderModuleDiff diff model.modules name

                                _ ->
                                    [ Tui.text "No module selected" ]
                    in
                    magnitudeHeader ++ entryDiffLines

                Nothing ->
                    [ Tui.text "No diff data" ]
    in
    Layout.pane "docs"
        { title = "Diff"
        , width = Layout.fill
        }
        (Layout.content diffLines)



-- DOCS RENDERING


renderModuleDocs : Docs.Module -> List Tui.Screen
renderModuleDocs mod =
    Docs.toBlocks mod
        |> List.concatMap
            (\block ->
                case block of
                    Docs.MarkdownBlock _ ->
                        []

                    _ ->
                        renderBlock block ++ [ Tui.text "" ]
            )


renderBlock : Docs.Block -> List Tui.Screen
renderBlock block =
    case block of
        Docs.ValueBlock value ->
            renderValueTui value

        Docs.UnionBlock union ->
            renderUnionTui union

        Docs.AliasBlock alias_ ->
            renderAliasTui alias_

        Docs.BinopBlock binop ->
            renderBinopTui binop

        Docs.MarkdownBlock _ ->
            []

        Docs.UnknownBlock name ->
            [ Tui.text ("  (unknown: " ++ name ++ ")") ]


renderValueTui : Docs.Value -> List Tui.Screen
renderValueTui { name, comment, tipe } =
    let
        typeStr =
            Render.typeToString tipe

        header =
            Tui.concat
                [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } name
                , Tui.text " : "
                , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] } typeStr
                ]

        commentLines =
            renderDocComment comment
    in
    header :: commentLines


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

        typeBody =
            Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] }
                ("    " ++ Render.typeToString tipe)

        commentLines =
            renderDocComment comment
    in
    header :: typeBody :: commentLines


renderBinopTui : Docs.Binop -> List Tui.Screen
renderBinopTui { name, comment, tipe } =
    let
        displayName =
            "(" ++ name ++ ")"

        header =
            Tui.concat
                [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ] } displayName
                , Tui.text " : "
                , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [] } (Render.typeToString tipe)
                ]

        commentLines =
            renderDocComment comment
    in
    header :: commentLines


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
            :: (trimmed
                    |> String.lines
                    |> List.map
                        (\line ->
                            Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] }
                                ("    " ++ line)
                        )
               )



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
    Tui.styled { fg = Just color, bg = Nothing, attributes = [ Tui.Bold ] }
        ("  " ++ magnitude ++ " CHANGE")


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
            [ Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ] }
                ("  New module: " ++ moduleName)
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
    [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ] }
        ("  Removed module: " ++ moduleName)
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
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ] } " + "

                Diff.Changed ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ] } " ~ "

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
subscriptions _ =
    Tui.Sub.batch
        [ Tui.Sub.onKeyPress KeyPressed
        , Tui.Sub.onMouse MouseEvent
        , Tui.Sub.onContext GotContext
        ]
