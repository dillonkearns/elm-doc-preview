module DiffTui exposing (Model, Msg, init, subscriptions, update, view)

{-| Lazygit-style TUI for browsing Elm API diffs.

Left pane: list of entries (README, modules) with diff status
Right pane: detailed diff view for the selected entry

-}

import Ansi.Color
import Dict
import Docs.Diff as Diff exposing (ApiDiff, ModuleChanges)
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
    , entries : List Entry
    , showHelp : Bool
    }


type Msg
    = KeyPressed Tui.KeyEvent
    | MouseEvent Tui.MouseEvent
    | SelectEntry Int


init : ApiDiff -> ( Model, Effect.Effect Msg )
init diff =
    let
        entries =
            buildEntries diff
    in
    ( { layout =
            Layout.init
                |> Layout.focusPane "modules"
      , diff = diff
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
            { termWidth = ctx.width, termHeight = ctx.height }
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


entriesPane : { a | width : Int, height : Int } -> Model -> Layout.Pane Msg
entriesPane _ model =
    let
        entryCount =
            List.length model.entries
    in
    Layout.pane "modules"
        { title = "Modules"
        , width = paneWidth model.layout "modules"
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
            , attributes = [ Tui.bold, Tui.inverse ]
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
            Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.bold ] } " ~ "

        ModuleEntry name ->
            case Diff.moduleStatus diff name of
                Diff.ModuleNew ->
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.bold ] } " + "

                Diff.ModuleChanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.bold ] } " ~ "

                Diff.ModuleRemoved ->
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.bold ] } " - "

                Diff.ModuleUnchanged ->
                    Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.bold ] } " ~ "


paneWidth : Layout.State -> String -> Layout.Width
paneWidth layoutState paneId =
    if Layout.focusedPane layoutState == Just paneId then
        Layout.fillPortion 2

    else
        Layout.fill


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
                    renderEntryDiff model.diff entry

                Nothing ->
                    [ Tui.text "No entry selected" ]

        diffLines =
            magnitudeHeader ++ entryDiffLines
    in
    Layout.pane "diff"
        { title = "Diff"
        , width = paneWidth model.layout "diff"
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
    Tui.styled { fg = Just color, bg = Nothing, attributes = [ Tui.bold ] }
        ("  " ++ magnitude ++ " CHANGE")


renderEntryDiff : ApiDiff -> Entry -> List Tui.Screen
renderEntryDiff diff entry =
    case entry of
        ReadmeEntry ->
            renderReadmeDiff diff

        ModuleEntry moduleName ->
            renderModuleDiff diff moduleName


renderReadmeDiff : ApiDiff -> List Tui.Screen
renderReadmeDiff diff =
    case diff.readmeDiff of
        Just rdiff ->
            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.bold ] } "  README Changes"
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
                    Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } ("    " ++ line)

                else if String.startsWith "- " line || String.startsWith "-" line then
                    Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [] } ("    " ++ line)

                else
                    Tui.styled { fg = Just Ansi.Color.brightBlack, bg = Nothing, attributes = [] } ("    " ++ line)
            )


renderModuleDiff : ApiDiff -> String -> List Tui.Screen
renderModuleDiff diff moduleName =
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

        isNewModule =
            List.member moduleName diff.addedModules

        isRemovedModule =
            List.member moduleName diff.removedModules

        header =
            if isNewModule then
                [ Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.bold ] }
                    ("  New module: " ++ moduleName)
                , Tui.text ""
                ]

            else if isRemovedModule then
                [ Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.bold ] }
                    ("  Removed module: " ++ moduleName)
                , Tui.text ""
                ]

            else
                []

        addedSection =
            if List.isEmpty addedItems then
                []

            else
                Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [ Tui.bold ] } "  Added:"
                    :: List.map
                        (\item ->
                            Tui.styled { fg = Just Ansi.Color.green, bg = Nothing, attributes = [] } ("    + " ++ item)
                        )
                        addedItems
                    ++ [ Tui.text "" ]

        changedSection =
            if List.isEmpty changedItems then
                []

            else
                Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [ Tui.bold ] } "  Changed:"
                    :: List.map
                        (\item ->
                            Tui.styled { fg = Just Ansi.Color.yellow, bg = Nothing, attributes = [] } ("    ~ " ++ item)
                        )
                        changedItems
                    ++ [ Tui.text "" ]

        removedSection =
            if List.isEmpty removedItems then
                []

            else
                Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [ Tui.bold ] } "  Removed:"
                    :: List.map
                        (\item ->
                            Tui.styled { fg = Just Ansi.Color.red, bg = Nothing, attributes = [] } ("    - " ++ item)
                        )
                        removedItems
                    ++ [ Tui.text "" ]

        commentDiffSection =
            renderCommentDiffs diff moduleName

        allSections =
            header ++ addedSection ++ changedSection ++ removedSection ++ commentDiffSection
    in
    if List.isEmpty allSections then
        [ Tui.text "  No changes in this module" ]

    else
        allSections


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
                Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.bold ] } "  Doc changes:"
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
                            Tui.styled { fg = Nothing, bg = Nothing, attributes = [ Tui.bold ] } ("    " ++ label ++ ":")
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
        ]
