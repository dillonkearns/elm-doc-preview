module DiffTui exposing (Model, Msg, init, subscriptions, update, view)

{-| Lazygit-style TUI for browsing Elm API diffs.

Left pane: list of entries (README, modules) with diff status
Right pane: detailed diff view for the selected entry, with full
rendered type signatures, doc comments, and unified diffs.

-}

import Ansi.Color
import Dict
import Docs.Diff as Diff exposing (ApiDiff, ModuleChanges)
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import Json.Decode as Decode
import Tui
import Tui.Effect as Effect
import Tui.Keybinding as Keybinding
import Tui.Layout as Layout
import Tui.Modal as Modal
import Tui.Sub


type Entry
    = ReadmeEntry
    | ModuleEntry String


type alias Model =
    { layout : Layout.State
    , diff : ApiDiff
    , modules : List Docs.Module
    , entries : List Entry
    , showHelp : Bool
    }


type Msg
    = KeyPressed Tui.KeyEvent
    | MouseEvent Tui.MouseEvent
    | SelectEntry Int
    | GotContext { width : Int, height : Int }


init : { diff : ApiDiff, modules : List Docs.Module } -> ( Model, Effect.Effect Msg )
init { diff, modules } =
    let
        entries =
            buildEntries diff
    in
    ( { layout =
            Layout.init
                |> Layout.focusPane "modules"
      , diff = diff
      , modules = modules
      , entries = entries
      , showHelp = False
      }
    , Effect.none
    )


buildEntries : ApiDiff -> List Entry
buildEntries diff =
    let
        readmeEntry =
            case diff.readmeDiff of
                Just _ ->
                    [ ReadmeEntry ]

                Nothing ->
                    []

        added =
            List.map ModuleEntry diff.addedModules

        changed =
            List.map (\mc -> ModuleEntry mc.name) diff.changedModules

        commentOnly =
            diff.commentDiffs
                |> Dict.keys
                |> List.filter
                    (\moduleName ->
                        not (List.any (\mc -> mc.name == moduleName) diff.changedModules)
                            && not (List.member moduleName diff.addedModules)
                    )
                |> List.map ModuleEntry

        removed =
            List.map ModuleEntry diff.removedModules
    in
    readmeEntry ++ added ++ changed ++ commentOnly ++ removed


selectedEntry : Model -> Maybe Entry
selectedEntry model =
    let
        idx =
            Layout.selectedIndex "modules" model.layout
    in
    model.entries
        |> List.drop idx
        |> List.head


entryName : Entry -> String
entryName entry =
    case entry of
        ReadmeEntry ->
            "README"

        ModuleEntry name ->
            name


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


update : Msg -> Model -> ( Model, Effect.Effect Msg )
update msg model =
    case msg of
        KeyPressed event ->
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
                        |> Layout.resetScroll "diff"
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
    | FocusDiff
    | ScrollDiffDown
    | ScrollDiffUp
    | ToggleHelp
    | CloseHelp


handleAction : Action -> Model -> ( Model, Effect.Effect Msg )
handleAction action model =
    case action of
        Quit ->
            ( model, Effect.exit )

        NavigateDown ->
            let
                ctx =
                    Layout.contextOf model.layout

                ( newLayout, _ ) =
                    Layout.navigateDown "modules" (viewLayout ctx model) model.layout
            in
            ( { model | layout = newLayout }
            , Effect.none
            )

        NavigateUp ->
            let
                ctx =
                    Layout.contextOf model.layout

                ( newLayout, _ ) =
                    Layout.navigateUp "modules" (viewLayout ctx model) model.layout
            in
            ( { model | layout = newLayout }
            , Effect.none
            )

        FocusModules ->
            ( { model | layout = Layout.focusPane "modules" model.layout }
            , Effect.none
            )

        FocusDiff ->
            ( { model | layout = Layout.focusPane "diff" model.layout }
            , Effect.none
            )

        ScrollDiffDown ->
            ( { model | layout = Layout.scrollDown "diff" 3 model.layout }
            , Effect.none
            )

        ScrollDiffUp ->
            ( { model | layout = Layout.scrollUp "diff" 3 model.layout }
            , Effect.none
            )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Effect.none )

        CloseHelp ->
            ( { model | showHelp = False }, Effect.none )


globalBindings : Keybinding.Group Action
globalBindings =
    Keybinding.group "Global"
        [ Keybinding.binding (Tui.Character 'q') "Quit" Quit
        , Keybinding.binding Tui.Escape "Quit" Quit
        , Keybinding.binding (Tui.Character '?') "Toggle help" ToggleHelp
        , Keybinding.binding (Tui.Character 'h') "Focus modules" FocusModules
            |> Keybinding.withAlternate (Tui.Arrow Tui.Left)
        , Keybinding.binding (Tui.Character 'l') "Focus diff" FocusDiff
            |> Keybinding.withAlternate (Tui.Arrow Tui.Right)
        ]


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
        ]


diffBindings : Keybinding.Group Action
diffBindings =
    Keybinding.group "Diff"
        [ Keybinding.binding (Tui.Character 'j') "Scroll down" ScrollDiffDown
            |> Keybinding.withAlternate (Tui.Arrow Tui.Down)
        , Keybinding.binding (Tui.Character 'k') "Scroll up" ScrollDiffUp
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
                    Just "diff" ->
                        [ diffBindings ]

                    _ ->
                        [ modulesBindings ]
        in
        focusedBindings ++ [ globalBindings ]



-- VIEW


view : Tui.Context -> Model -> Tui.Screen
view ctx model =
    let
        layoutState =
            Layout.withContext { width = ctx.width, height = ctx.height } model.layout
    in
    if model.showHelp then
        let
            bgRows =
                Layout.toRows layoutState (viewLayout ctx model)

            helpBody =
                Keybinding.helpRows "" [ modulesBindings, diffBindings, globalBindings ]
        in
        Modal.overlay
            { title = "Keybindings"
            , body = helpBody
            , footer = "? close  q quit"
            , width = min 50 (ctx.width - 4)
            }
            { width = ctx.width, height = ctx.height }
            bgRows
            |> Tui.lines

    else
        viewLayout ctx model
            |> Layout.toScreen layoutState


viewLayout : { a | width : Int, height : Int } -> Model -> Layout.Layout Msg
viewLayout ctx model =
    Layout.horizontal
        [ entriesPane ctx model
        , diffPane ctx model
        ]


modulesPaneWidth : Int -> Layout.Width
modulesPaneWidth termWidth =
    Layout.fixed (min 40 (termWidth // 3))


entriesPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
entriesPane ctx model =
    let
        entryCount =
            List.length model.entries
    in
    Layout.pane "modules"
        { title = "Modules"
        , width = modulesPaneWidth ctx.width
        }
        (Layout.selectableList
            { onSelect = SelectEntry
            , selected = \entry -> renderEntrySelected model.diff entry
            , default = \entry -> renderEntryDefault model.diff entry
            }
            model.entries
        )
        |> Layout.withPrefix ("[" ++ String.fromInt entryCount ++ "] ")
        |> Layout.withFooter
            (String.fromInt (Layout.selectedIndex "modules" model.layout + 1)
                ++ " of "
                ++ String.fromInt entryCount
            )


renderEntrySelected : ApiDiff -> Entry -> Tui.Screen
renderEntrySelected diff entry =
    Tui.concat
        [ entryBadge diff entry
        , Tui.styled
            { fg = Nothing
            , bg = Nothing
            , attributes = [ Tui.Bold, Tui.Inverse ]
            , hyperlink = Nothing
            }
            (" " ++ entryName entry ++ " ")
        ]


renderEntryDefault : ApiDiff -> Entry -> Tui.Screen
renderEntryDefault diff entry =
    Tui.concat
        [ entryBadge diff entry
        , Tui.text (" " ++ entryName entry)
        ]


entryBadge : ApiDiff -> Entry -> Tui.Screen
entryBadge diff entry =
    case entry of
        ReadmeEntry ->
            Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "

        ModuleEntry name ->
            case Diff.moduleStatus diff name of
                Diff.ModuleNew ->
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " + "

                Diff.ModuleChanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "

                Diff.ModuleRemoved ->
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " - "

                Diff.ModuleUnchanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "


diffPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
diffPane _ model =
    let
        magnitudeHeader =
            [ magnitudeLine model.diff.magnitude
            , Tui.text ""
            ]

        entryDiffLines =
            case selectedEntry model of
                Just entry ->
                    renderEntryDiff model.diff model.modules entry

                Nothing ->
                    [ Tui.text "No entry selected" ]

        diffLines =
            magnitudeHeader ++ entryDiffLines
    in
    Layout.pane "diff"
        { title = "Diff"
        , width = Layout.fill
        }
        (Layout.content diffLines)


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
    Tui.styled { fg = Just color, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
        ("  " ++ magnitude ++ " CHANGE")


renderEntryDiff : ApiDiff -> List Docs.Module -> Entry -> List Tui.Screen
renderEntryDiff diff modules entry =
    case entry of
        ReadmeEntry ->
            renderReadmeDiff diff

        ModuleEntry moduleName ->
            renderModuleDiff diff modules moduleName


renderReadmeDiff : ApiDiff -> List Tui.Screen
renderReadmeDiff diff =
    case diff.readmeDiff of
        Just rdiff ->
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } "  README Changes"
                :: Tui.text ""
                :: renderUnifiedDiffLines rdiff

        Nothing ->
            [ Tui.text "  No README changes" ]


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


renderNewModule : String -> Maybe Docs.Module -> List Tui.Screen
renderNewModule moduleName maybeModule =
    let
        header =
            [ Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
                ("  New module: " ++ moduleName)
            , Tui.text ""
            ]
    in
    case maybeModule of
        Just mod ->
            header ++ renderAllBlocks mod

        Nothing ->
            header


renderRemovedModule : String -> List Tui.Screen
renderRemovedModule moduleName =
    [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
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


renderItemBlock : ApiDiff -> String -> String -> Diff.DiffStatus -> Maybe Docs.Module -> List Tui.Screen
renderItemBlock diff moduleName itemName status maybeModule =
    let
        badge =
            case status of
                Diff.Added ->
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " + "

                Diff.Changed ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } " ~ "

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
                    [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.Strikethrough ], hyperlink = Nothing }
                        ("   " ++ itemName ++ " : " ++ Render.typeToString tipe)
                    ]

                Err _ ->
                    []

        Nothing ->
            []


findBlock : String -> Docs.Module -> Maybe Docs.Block
findBlock name mod =
    let
        blocks =
            Docs.toBlocks mod
    in
    blocks
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



-- BLOCK RENDERING TO TUI SCREENS


renderAllBlocks : Docs.Module -> List Tui.Screen
renderAllBlocks mod =
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
                [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } name
                , Tui.text " : "
                , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing } typeStr
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
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing }
                ("type alias " ++ name ++ typeVars ++ " =")

        typeBody =
            Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing }
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
                [ Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.Bold ], hyperlink = Nothing } displayName
                , Tui.text " : "
                , Tui.styled { fg = Just Ansi.Color.cyan, bg = Nothing, attributes = [], hyperlink = Nothing } (Render.typeToString tipe)
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
                            Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [], hyperlink = Nothing }
                                ("    " ++ line)
                        )
               )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Tui.Sub.Sub Msg
subscriptions _ =
    Tui.Sub.batch
        [ Tui.Sub.onKeyPress KeyPressed
        , Tui.Sub.onMouse MouseEvent
        , Tui.Sub.onContext GotContext
        ]
