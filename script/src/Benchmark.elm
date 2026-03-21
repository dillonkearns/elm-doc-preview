module Benchmark exposing (run)

{-| Benchmark for module docs rendering performance.

Run from a package directory with docs.json:

    elm make --docs=docs.json
    node --prof $(elm-pages bundle-script src/Benchmark.elm --output /dev/stdout 2>/dev/null)

Or simply:

    elm-pages run src/Benchmark.elm

The script renders each module's docs using the same pipeline as DocsTui
and reports the number of items, rendered lines, and enables Node.js
profiling to identify bottlenecks.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.File
import Docs.Render as Render
import Elm.Docs as Docs
import Elm.Type as Type
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import ModuleTree
import Pages.Script as Script exposing (Script)
import SyntaxHighlight
import Tui


run : Script
run =
    Script.withoutCliOptions
        (BackendTask.File.rawFile "docs.json"
            |> BackendTask.allowFatal
            |> BackendTask.andThen
                (\json ->
                    case Decode.decodeString (Decode.list Docs.decoder) json of
                        Ok modules ->
                            let
                                sorted =
                                    modules
                                        |> List.sortBy (\m -> negate (itemCount m))

                                top5 =
                                    List.take 5 sorted
                            in
                            Script.log ("Found " ++ String.fromInt (List.length modules) ++ " modules\n")
                                |> BackendTask.andThen (\() -> benchmarkSequential top5)

                        Err err ->
                            Script.log ("Parse error: " ++ Decode.errorToString err)
                )
        )


benchmarkSequential : List Docs.Module -> BackendTask FatalError ()
benchmarkSequential modules =
    case modules of
        [] ->
            Script.log "\nDone."

        mod :: rest ->
            let
                wrapWidth =
                    100

                blocks =
                    Docs.toBlocks mod

                blockCount =
                    List.length blocks

                -- PHASE 1: Just toBlocks
                _ =
                    blocks

                -- PHASE 2: Render all blocks (the expensive part)
                rendered =
                    blocks
                        |> List.concatMap
                            (\block ->
                                renderBlockFull wrapWidth block ++ [ Tui.text "" ]
                            )

                lineCount =
                    List.length rendered

                -- PHASE 3: Tui.toString on all lines
                totalChars =
                    rendered
                        |> List.map (Tui.toString >> String.length)
                        |> List.sum

                -- PHASE 4: findAllItemLines
                items =
                    moduleItemNames mod

                positions =
                    findAllItemLines items rendered
            in
            Script.log
                (mod.name
                    ++ ": "
                    ++ String.fromInt (itemCount mod)
                    ++ " items, "
                    ++ String.fromInt blockCount
                    ++ " blocks → "
                    ++ String.fromInt lineCount
                    ++ " lines, "
                    ++ String.fromInt totalChars
                    ++ " chars, "
                    ++ String.fromInt (List.length positions)
                    ++ " positions"
                )
                |> BackendTask.andThen (\() -> benchmarkSequential rest)



-- Rendering pipeline (mirrors DocsTui exactly)


renderBlockFull : Int -> Docs.Block -> List Tui.Screen
renderBlockFull wrapWidth block =
    case block of
        Docs.ValueBlock value ->
            renderValueFull wrapWidth value
                |> addGutter Ansi.Color.brightBlack

        Docs.UnionBlock union ->
            renderUnionFull wrapWidth union
                |> addGutter Ansi.Color.brightBlack

        Docs.AliasBlock alias_ ->
            renderAliasFull wrapWidth alias_
                |> addGutter Ansi.Color.brightBlack

        Docs.BinopBlock binop ->
            renderBinopFull wrapWidth binop
                |> addGutter Ansi.Color.brightBlack

        Docs.MarkdownBlock markdown ->
            renderMarkdown (String.trim markdown)
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


renderValueFull : Int -> Docs.Value -> List Tui.Screen
renderValueFull wrapWidth { name, comment, tipe } =
    let
        prefixWidth =
            String.length name + 3

        typeLines =
            Render.typeToLines prefixWidth tipe

        header =
            case typeLines of
                [ oneLine ] ->
                    [ Tui.concat
                        [ Tui.text name |> Tui.bold
                        , Tui.text " : "
                        , Tui.text oneLine |> Tui.fg Ansi.Color.cyan
                        ]
                    ]

                multipleLines ->
                    (Tui.concat [ Tui.text name |> Tui.bold, Tui.text " :" ])
                        :: List.map (\line -> Tui.text ("    " ++ line) |> Tui.fg Ansi.Color.cyan) multipleLines

        commentLines =
            renderDocComment wrapWidth comment
    in
    header ++ commentLines


renderUnionFull : Int -> Docs.Union -> List Tui.Screen
renderUnionFull wrapWidth { name, comment, args, tags } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.text ("type " ++ name ++ typeVars) |> Tui.bold

        commentLines =
            renderDocComment wrapWidth comment
    in
    header :: commentLines


renderAliasFull : Int -> Docs.Alias -> List Tui.Screen
renderAliasFull wrapWidth { name, comment, args, tipe } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            Tui.text ("type alias " ++ name ++ typeVars ++ " =") |> Tui.bold

        commentLines =
            renderDocComment wrapWidth comment
    in
    header :: commentLines


renderBinopFull : Int -> Docs.Binop -> List Tui.Screen
renderBinopFull wrapWidth { name, comment, tipe } =
    let
        header =
            Tui.concat
                [ Tui.text ("(" ++ name ++ ")") |> Tui.bold
                , Tui.text " : "
                , Tui.text (Render.typeToString tipe) |> Tui.fg Ansi.Color.cyan
                ]

        commentLines =
            renderDocComment wrapWidth comment
    in
    header :: commentLines


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
            :: (renderMarkdown trimmed |> wrapLines commentWidth)
            |> List.map (\line -> Tui.concat [ Tui.text "    ", line ])

    else
        Tui.text ""
            :: (trimmed |> String.lines |> List.map Tui.text |> wrapLines commentWidth)
            |> List.map (\line -> Tui.concat [ Tui.text "    ", line ])


renderMarkdown : String -> List Tui.Screen
renderMarkdown markdown =
    case
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "parse error")
            |> Result.andThen (Markdown.Renderer.render simpleRenderer)
    of
        Ok rendered ->
            List.concat rendered

        Err _ ->
            markdown |> String.lines |> List.map Tui.text


simpleRenderer : Markdown.Renderer.Renderer (List Tui.Screen)
simpleRenderer =
    { heading = \{ children } -> [ Tui.text (inlineText children) |> Tui.bold ]
    , paragraph = \children -> [ Tui.concat (List.concat children), Tui.text "" ]
    , blockQuote = \children -> List.concat children
    , html = Markdown.Html.oneOf []
    , text = \s -> [ Tui.text (String.replace "\n" " " s) ]
    , codeSpan = \code -> [ Tui.text code |> Tui.fg Ansi.Color.cyan ]
    , strong = \children -> [ Tui.text (inlineText children) |> Tui.bold ]
    , emphasis = \children -> [ Tui.text (inlineText children) ]
    , strikethrough = \children -> [ Tui.text (inlineText children) ]
    , hardLineBreak = [ Tui.text "" ]
    , link = \_ children -> List.concat children
    , image = \{ alt } -> [ Tui.text ("[" ++ alt ++ "]") ]
    , unorderedList = \items -> List.concatMap (\(ListItem _ children) -> List.concat children) items
    , orderedList = \_ items -> List.concatMap List.concat items
    , codeBlock = \{ body } -> body |> String.lines |> List.map Tui.text
    , thematicBreak = [ Tui.text "---" ]
    , table = \children -> List.concat children
    , tableHeader = \children -> List.concat children
    , tableBody = \children -> List.concat children
    , tableRow = \children -> [ Tui.concat (List.concat children) ]
    , tableCell = \_ children -> List.concat children
    , tableHeaderCell = \_ children -> List.concat children
    }


inlineText : List (List Tui.Screen) -> String
inlineText children =
    children |> List.concat |> List.map Tui.toString |> String.join ""


wrapLines : Int -> List Tui.Screen -> List Tui.Screen
wrapLines maxWidth lines =
    if maxWidth <= 0 then
        lines

    else
        List.concatMap (Tui.wrapWidth maxWidth) lines


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
                    Just ("# " ++ String.trim (String.dropLeft 3 (String.trim line)))

                else
                    Nothing
            )


findAllItemLines : List String -> List Tui.Screen -> List ( String, Int )
findAllItemLines items lines =
    let
        indexedTextLines =
            lines
                |> List.indexedMap (\idx line -> ( idx, Tui.toString line ))

        findItem itemName =
            let
                matchLine ( _, text ) =
                    if String.startsWith "# " itemName then
                        String.contains ("## " ++ String.dropLeft 2 itemName) text

                    else
                        String.contains ("┃ " ++ itemName ++ " :") text
                            || String.contains ("┃ " ++ itemName ++ " =") text
                            || String.contains ("┃ type " ++ itemName ++ " ") text
                            || String.contains ("┃ type alias " ++ itemName ++ " ") text
            in
            indexedTextLines
                |> List.filter matchLine
                |> List.head
                |> Maybe.map (\( idx, _ ) -> ( itemName, idx ))
                |> Maybe.withDefault ( itemName, 0 )
    in
    List.map findItem items


itemCount : Docs.Module -> Int
itemCount mod =
    List.length mod.values
        + List.length mod.unions
        + List.length mod.aliases
        + List.length mod.binops
