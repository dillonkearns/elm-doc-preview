module Docs.Render exposing
    ( typeToString
    , renderValue
    , renderUnion
    , renderAlias
    , renderBinop
    , renderToc
    , renderModule
    , renderBlock
    )

import Ansi.Color
import Ansi.Font
import Docs.Block as Block
import Elm.Docs as Docs
import Elm.Type as Type
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer)
import SyntaxHighlight


maxWidth : Int
maxWidth =
    64


{-| Convert an Elm Type to a readable string, handling parenthesization.
-}
typeToString : Type.Type -> String
typeToString tipe =
    typeToStringHelp False tipe


typeToStringHelp : Bool -> Type.Type -> String
typeToStringHelp needsParens tipe =
    case tipe of
        Type.Var name ->
            name

        Type.Lambda arg result ->
            let
                inner =
                    arrowChain arg result
            in
            if needsParens then
                "(" ++ inner ++ ")"

            else
                inner

        Type.Tuple [] ->
            "()"

        Type.Tuple types ->
            "( " ++ String.join ", " (List.map (typeToStringHelp False) types) ++ " )"

        Type.Type name args ->
            let
                shortName =
                    toShortName name
            in
            case args of
                [] ->
                    shortName

                _ ->
                    let
                        inner =
                            shortName ++ " " ++ String.join " " (List.map (typeToStringHelp True) args)
                    in
                    if needsParens then
                        "(" ++ inner ++ ")"

                    else
                        inner

        Type.Record fields extension ->
            case ( fields, extension ) of
                ( [], Nothing ) ->
                    "{}"

                ( [], Just ext ) ->
                    "{ " ++ ext ++ " | }"

                ( _, Nothing ) ->
                    "{ " ++ String.join ", " (List.map fieldToString fields) ++ " }"

                ( _, Just ext ) ->
                    "{ " ++ ext ++ " | " ++ String.join ", " (List.map fieldToString fields) ++ " }"


arrowChain : Type.Type -> Type.Type -> String
arrowChain arg result =
    let
        argStr =
            typeToStringHelp (isLambda arg) arg

        resultParts =
            collectArrowParts result
    in
    String.join " -> " (argStr :: resultParts)


collectArrowParts : Type.Type -> List String
collectArrowParts tipe =
    case tipe of
        Type.Lambda arg result ->
            typeToStringHelp (isLambda arg) arg :: collectArrowParts result

        _ ->
            [ typeToStringHelp False tipe ]


isLambda : Type.Type -> Bool
isLambda tipe =
    case tipe of
        Type.Lambda _ _ ->
            True

        _ ->
            False


fieldToString : ( String, Type.Type ) -> String
fieldToString ( name, tipe ) =
    name ++ " : " ++ typeToStringHelp False tipe


toShortName : String -> String
toShortName qualifiedName =
    case List.reverse (String.split "." qualifiedName) of
        last :: _ ->
            last

        [] ->
            qualifiedName



-- MULTI-LINE TYPE FORMATTING


{-| Format a type signature with line breaks when it exceeds maxWidth.
Returns a list of lines. The first line is meant to follow " : " on the
name line. Subsequent lines are indented.
-}
typeToLines : Int -> Type.Type -> List String
typeToLines prefixWidth tipe =
    let
        oneLine =
            typeToString tipe
    in
    if prefixWidth + String.length oneLine <= maxWidth then
        [ oneLine ]

    else
        typeToMultiLines "    " tipe


typeToMultiLines : String -> Type.Type -> List String
typeToMultiLines indent tipe =
    case tipe of
        Type.Lambda arg result ->
            let
                allArgs =
                    collectLambdaArgs arg result

                renderArg i argType =
                    let
                        argStr =
                            typeToStringHelp (isLambda argType) argType

                        prefix =
                            if i == 0 then
                                ""

                            else
                                indent ++ "-> "
                    in
                    if String.length prefix + String.length argStr <= maxWidth then
                        [ prefix ++ argStr ]

                    else
                        case argType of
                            Type.Record fields ext ->
                                multiLineRecord (prefix ++ "{ ") (indent ++ "   ") fields ext

                            _ ->
                                [ prefix ++ argStr ]
            in
            List.indexedMap renderArg allArgs
                |> List.concat

        Type.Record fields ext ->
            multiLineRecord "{ " indent fields ext

        _ ->
            [ typeToString tipe ]


collectLambdaArgs : Type.Type -> Type.Type -> List Type.Type
collectLambdaArgs arg result =
    case result of
        Type.Lambda nextArg nextResult ->
            arg :: collectLambdaArgs nextArg nextResult

        _ ->
            [ arg, result ]


multiLineRecord : String -> String -> List ( String, Type.Type ) -> Maybe String -> List String
multiLineRecord openPrefix indent fields extension =
    case fields of
        [] ->
            case extension of
                Nothing ->
                    [ "{}" ]

                Just ext ->
                    [ "{ " ++ ext ++ " | }" ]

        first :: rest ->
            let
                extPrefix =
                    case extension of
                        Nothing ->
                            openPrefix

                        Just ext ->
                            "{ " ++ ext ++ " | "

                firstLine =
                    extPrefix ++ fieldToString first

                restLines =
                    List.map (\f -> ", " ++ fieldToString f) rest

                closeLine =
                    "}"
            in
            firstLine :: restLines ++ [ closeLine ]



-- VALUE RENDERING


{-| Render a value/function documentation entry.
-}
renderValue : Docs.Value -> String
renderValue { name, comment, tipe } =
    let
        prefixWidth =
            String.length name + 3

        header =
            case typeToLines prefixWidth tipe of
                [ oneLine ] ->
                    "  " ++ Ansi.Font.bold name ++ " : " ++ Ansi.Color.fontColor Ansi.Color.cyan oneLine

                multipleLines ->
                    multipleLines
                        |> List.map (\line -> "      " ++ Ansi.Color.fontColor Ansi.Color.cyan line)
                        |> String.join "\n"
                        |> (\typeStr -> "  " ++ Ansi.Font.bold name ++ " :\n" ++ typeStr)

        body =
            indentComment comment
    in
    header ++ "\n\n" ++ body



-- UNION RENDERING


{-| Render a union type documentation entry.
-}
renderUnion : Docs.Union -> String
renderUnion { name, comment, args, tags } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            "  " ++ Ansi.Font.bold ("type " ++ name ++ typeVars)

        constructors =
            case tags of
                [] ->
                    ""

                first :: rest ->
                    "\n"
                        ++ "      = "
                        ++ renderTag first
                        ++ String.concat (List.map (\t -> "\n      | " ++ renderTag t) rest)

        body =
            indentComment comment
    in
    header ++ constructors ++ "\n\n" ++ body


renderTag : ( String, List Type.Type ) -> String
renderTag ( tagName, tagArgs ) =
    case tagArgs of
        [] ->
            tagName

        _ ->
            tagName ++ " " ++ String.join " " (List.map (typeToStringHelp True) tagArgs)



-- ALIAS RENDERING


{-| Render a type alias documentation entry.
-}
renderAlias : Docs.Alias -> String
renderAlias { name, comment, args, tipe } =
    let
        typeVars =
            case args of
                [] ->
                    ""

                _ ->
                    " " ++ String.join " " args

        header =
            "  " ++ Ansi.Font.bold ("type alias " ++ name ++ typeVars) ++ " ="

        typeBody =
            case typeToLines 6 tipe of
                [ oneLine ] ->
                    "      " ++ oneLine

                multipleLines ->
                    multipleLines
                        |> List.map (\line -> "      " ++ line)
                        |> String.join "\n"

        body =
            indentComment comment
    in
    header ++ "\n" ++ typeBody ++ "\n\n" ++ body



-- BINOP RENDERING


{-| Render a binary operator documentation entry.
-}
renderBinop : Docs.Binop -> String
renderBinop { name, comment, tipe, associativity, precedence } =
    let
        displayName =
            "(" ++ name ++ ")"

        prefixWidth =
            String.length displayName + 3

        header =
            case typeToLines prefixWidth tipe of
                [ oneLine ] ->
                    "  " ++ Ansi.Font.bold displayName ++ " : " ++ Ansi.Color.fontColor Ansi.Color.cyan oneLine

                multipleLines ->
                    multipleLines
                        |> List.map (\line -> "      " ++ Ansi.Color.fontColor Ansi.Color.cyan line)
                        |> String.join "\n"
                        |> (\typeStr -> "  " ++ Ansi.Font.bold displayName ++ " :\n" ++ typeStr)

        assocStr =
            case associativity of
                Docs.Left ->
                    "left"

                Docs.Right ->
                    "right"

                Docs.None ->
                    "non"

        info =
            "    " ++ assocStr ++ "-associative, precedence " ++ String.fromInt precedence

        body =
            indentComment comment
    in
    header ++ "\n" ++ info ++ "\n\n" ++ body



-- TOC RENDERING


{-| Render a table of contents listing all modules.
-}
renderToc : List Docs.Module -> String
renderToc modules =
    let
        maxNameLen =
            modules
                |> List.map (.name >> String.length)
                |> List.maximum
                |> Maybe.withDefault 0

        renderRow mod =
            let
                padding =
                    String.repeat (maxNameLen - String.length mod.name + 2) " "

                summary =
                    firstSentence mod.comment
            in
            "  " ++ Ansi.Font.bold mod.name ++ padding ++ summary

        rows =
            List.map renderRow modules

        header =
            Ansi.Font.bold "── Modules ──"

        footer =
            "\nUse --module <name> to view a module's docs."
    in
    header ++ "\n\n" ++ String.join "\n" rows ++ "\n" ++ footer


{-| Extract the first sentence from a doc comment.
Stops at the first period followed by a space, newline, or end of string.
Ignores @docs directives.
-}
firstSentence : String -> String
firstSentence comment =
    let
        trimmed =
            String.trim comment

        -- Take only text before first @docs directive
        beforeDocs =
            if String.startsWith "@docs " trimmed then
                ""

            else
                case String.indexes "\n@docs " trimmed of
                    idx :: _ ->
                        String.trim (String.left idx trimmed)

                    [] ->
                        trimmed
    in
    case String.indexes ". " beforeDocs of
        idx :: _ ->
            String.left (idx + 1) beforeDocs

        [] ->
            case String.indexes ".\n" beforeDocs of
                idx :: _ ->
                    String.left (idx + 1) beforeDocs

                [] ->
                    if String.endsWith "." beforeDocs then
                        beforeDocs

                    else
                        beforeDocs



-- MODULE RENDERING


{-| Render a complete module view with header and all blocks.
-}
renderModule : Docs.Module -> String
renderModule module_ =
    let
        header =
            Ansi.Font.bold ("── " ++ module_.name ++ " " ++ String.repeat (40 - String.length module_.name) "─")

        blocks =
            Block.toBlocks module_

        renderedBlocks =
            List.map renderBlock blocks
    in
    header ++ "\n\n" ++ String.join "\n\n" renderedBlocks ++ "\n"


{-| Render a single documentation block.
-}
renderBlock : Docs.Block -> String
renderBlock block =
    case block of
        Docs.MarkdownBlock markdown ->
            renderMarkdown (String.trim markdown)

        Docs.ValueBlock value ->
            renderValue value

        Docs.BinopBlock binop ->
            renderBinop binop

        Docs.AliasBlock alias_ ->
            renderAlias alias_

        Docs.UnionBlock union ->
            renderUnion union

        Docs.UnknownBlock name ->
            "  (unknown: " ++ name ++ ")"



-- MARKDOWN RENDERING


{-| Render markdown text to ANSI-formatted terminal output.
-}
renderMarkdown : String -> String
renderMarkdown text =
    case
        text
            |> Markdown.Parser.parse
            |> Result.mapError (\errors -> List.map Markdown.Parser.deadEndToString errors |> String.join "\n")
            |> Result.andThen (Markdown.Renderer.render ansiRenderer)
    of
        Ok rendered ->
            String.join "\n" rendered

        Err _ ->
            -- Fallback: return raw text
            text


{-| Render a doc comment as indented markdown.
-}
indentComment : String -> String
indentComment comment =
    comment
        |> String.trim
        |> renderMarkdown
        |> String.lines
        |> List.map
            (\line ->
                if String.isEmpty (String.trim line) then
                    ""

                else
                    "    " ++ line
            )
        |> String.join "\n"



-- ANSI MARKDOWN RENDERER


ansiRenderer : Renderer String
ansiRenderer =
    { heading = heading
    , paragraph = \children -> String.join "" children ++ "\n"
    , blockQuote = blockQuote
    , html = Markdown.Html.oneOf []
    , text = identity
    , codeSpan = \code -> Ansi.Color.fontColor Ansi.Color.cyan code
    , strong = \children -> Ansi.Font.bold (String.join "" children)
    , emphasis = \children -> Ansi.Font.italic (String.join "" children)
    , strikethrough = \children -> Ansi.Font.strikeThrough (String.join "" children)
    , hardLineBreak = "\n"
    , link = link
    , image = image
    , unorderedList = unorderedList
    , orderedList = orderedList
    , codeBlock = codeBlock
    , thematicBreak = thematicBreak
    , table = \children -> String.join "" children ++ "\n"
    , tableHeader = \children -> String.join "" children ++ "\n"
    , tableBody = \children -> String.join "" children
    , tableRow = \children -> "| " ++ String.join " | " children ++ " |\n"
    , tableCell = \_ children -> String.join "" children
    , tableHeaderCell = \_ children -> Ansi.Font.bold (String.join "" children)
    }


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List String } -> String
heading { level, children } =
    let
        prefix =
            case level of
                Markdown.Block.H1 ->
                    "# "

                Markdown.Block.H2 ->
                    "## "

                Markdown.Block.H3 ->
                    "### "

                Markdown.Block.H4 ->
                    "#### "

                Markdown.Block.H5 ->
                    "##### "

                Markdown.Block.H6 ->
                    "###### "

        content =
            String.join "" children
    in
    "\n" ++ Ansi.Font.bold (Ansi.Color.fontColor Ansi.Color.magenta (prefix ++ content)) ++ "\n"


blockQuote : List String -> String
blockQuote children =
    children
        |> String.join ""
        |> String.lines
        |> List.map (\line -> Ansi.Color.fontColor Ansi.Color.brightBlack ("  > " ++ line))
        |> String.join "\n"
        |> (\s -> s ++ "\n")


link : { title : Maybe String, destination : String } -> List String -> String
link { destination } children =
    let
        label =
            String.join "" children
    in
    Ansi.Font.underline (Ansi.Color.fontColor Ansi.Color.blue label)
        ++ " ("
        ++ Ansi.Color.fontColor Ansi.Color.brightBlack destination
        ++ ")"


image : { alt : String, src : String, title : Maybe String } -> String
image { alt, src } =
    Ansi.Color.fontColor Ansi.Color.yellow ("[image: " ++ alt ++ "]")
        ++ " ("
        ++ Ansi.Color.fontColor Ansi.Color.brightBlack src
        ++ ")"


unorderedList : List (ListItem String) -> String
unorderedList items =
    items
        |> List.map
            (\(ListItem task children) ->
                let
                    bullet =
                        case task of
                            NoTask ->
                                Ansi.Color.fontColor Ansi.Color.green "  * "

                            IncompleteTask ->
                                Ansi.Color.fontColor Ansi.Color.yellow "  [ ] "

                            CompletedTask ->
                                Ansi.Color.fontColor Ansi.Color.green "  [x] "
                in
                bullet ++ String.join "" children
            )
        |> String.join "\n"
        |> (\s -> s ++ "\n")


orderedList : Int -> List (List String) -> String
orderedList startIndex items =
    items
        |> List.indexedMap
            (\i children ->
                let
                    num =
                        String.fromInt (startIndex + i)
                in
                "  "
                    ++ Ansi.Color.fontColor Ansi.Color.green (num ++ ". ")
                    ++ String.join "" children
            )
        |> String.join "\n"
        |> (\s -> s ++ "\n")


codeBlock : { body : String, language : Maybe String } -> String
codeBlock { body, language } =
    highlightCode "  " (language |> Maybe.withDefault "elm") body


thematicBreak : String
thematicBreak =
    Ansi.Color.fontColor Ansi.Color.brightBlack (String.repeat 40 "─") ++ "\n"



-- SYNTAX HIGHLIGHTING


highlightCode : String -> String -> String -> String
highlightCode indent language body =
    let
        trimmedBody =
            String.trimRight body

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
            trimmedBody
                |> String.lines
                |> List.map (\line -> indent ++ Ansi.Color.fontColor Ansi.Color.yellow line)
                |> String.join "\n"
                |> (\s -> s ++ "\n")
    in
    case parser of
        Just parse ->
            case parse trimmedBody of
                Ok hcode ->
                    hcode
                        |> SyntaxHighlight.toConsole consoleOptions
                        |> List.map (\line -> indent ++ String.replace "\n" "" line)
                        |> String.join "\n"
                        |> (\s -> s ++ "\n")

                Err _ ->
                    fallback

        Nothing ->
            fallback


consoleOptions : SyntaxHighlight.ConsoleOptions
consoleOptions =
    { default = Ansi.Color.fontColor Ansi.Color.white
    , highlight = Ansi.Color.backgroundColor Ansi.Color.brightBlack
    , addition = Ansi.Color.fontColor Ansi.Color.green
    , deletion = Ansi.Color.fontColor Ansi.Color.red
    , comment = Ansi.Color.fontColor Ansi.Color.brightBlack
    , style1 = Ansi.Color.fontColor Ansi.Color.cyan -- numbers
    , style2 = Ansi.Color.fontColor Ansi.Color.green -- strings
    , style3 = Ansi.Color.fontColor Ansi.Color.magenta -- keywords, tags
    , style4 = Ansi.Color.fontColor Ansi.Color.yellow -- group symbols
    , style5 = Ansi.Color.fontColor Ansi.Color.blue -- functions
    , style6 = Ansi.Color.fontColor Ansi.Color.red -- literal keywords
    , style7 = Ansi.Color.fontColor Ansi.Color.brightCyan -- arguments
    }
