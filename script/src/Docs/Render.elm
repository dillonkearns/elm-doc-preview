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
import SyntaxHighlight


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



-- VALUE RENDERING


{-| Render a value/function documentation entry.
-}
renderValue : Docs.Value -> String
renderValue { name, comment, tipe } =
    let
        header =
            "  " ++ Ansi.Font.bold name ++ " : " ++ Ansi.Color.fontColor Ansi.Color.cyan (typeToString tipe)

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
            "      " ++ typeToString tipe

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
        header =
            "  "
                ++ Ansi.Font.bold ("(" ++ name ++ ")")
                ++ " : "
                ++ Ansi.Color.fontColor Ansi.Color.cyan (typeToString tipe)

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


renderMarkdown : String -> String
renderMarkdown text =
    text
        |> String.lines
        |> groupCodeBlocks
        |> String.join "\n"


{-| Group consecutive indented lines into code blocks and highlight them.
Non-indented lines are rendered individually.
-}
groupCodeBlocks : List String -> List String
groupCodeBlocks lines =
    groupCodeBlocksHelp lines [] []


groupCodeBlocksHelp : List String -> List String -> List String -> List String
groupCodeBlocksHelp remaining codeAcc resultAcc =
    case remaining of
        [] ->
            List.reverse (flushCodeBlock codeAcc resultAcc)

        line :: rest ->
            if String.startsWith "    " line then
                groupCodeBlocksHelp rest (String.dropLeft 4 line :: codeAcc) resultAcc

            else
                let
                    flushed =
                        flushCodeBlock codeAcc resultAcc
                in
                groupCodeBlocksHelp rest [] (renderMarkdownLine line :: flushed)


flushCodeBlock : List String -> List String -> List String
flushCodeBlock codeAcc resultAcc =
    case codeAcc of
        [] ->
            resultAcc

        _ ->
            let
                codeText =
                    codeAcc |> List.reverse |> String.join "\n"

                highlighted =
                    highlightElm "    " codeText
            in
            highlighted :: resultAcc


renderMarkdownLine : String -> String
renderMarkdownLine line =
    if String.startsWith "# " line then
        Ansi.Font.bold (String.dropLeft 2 line)

    else if String.startsWith "## " line then
        Ansi.Font.bold (String.dropLeft 3 line)

    else if String.startsWith "### " line then
        Ansi.Font.bold (String.dropLeft 4 line)

    else
        line



-- HELPERS


indentComment : String -> String
indentComment comment =
    comment
        |> String.trim
        |> String.lines
        |> groupIndentedCodeBlocks
        |> String.join "\n"


{-| Process doc comment lines: indent prose with 4 spaces, and highlight
indented code blocks (4+ spaces) as Elm with syntax highlighting.
-}
groupIndentedCodeBlocks : List String -> List String
groupIndentedCodeBlocks lines =
    groupIndentedHelp lines [] []


groupIndentedHelp : List String -> List String -> List String -> List String
groupIndentedHelp remaining codeAcc resultAcc =
    case remaining of
        [] ->
            List.reverse (flushIndentedCodeBlock codeAcc resultAcc)

        line :: rest ->
            if String.isEmpty (String.trim line) then
                if List.isEmpty codeAcc then
                    groupIndentedHelp rest [] ("" :: resultAcc)

                else
                    -- Empty line inside a code block
                    groupIndentedHelp rest ("" :: codeAcc) resultAcc

            else if String.startsWith "    " line then
                groupIndentedHelp rest (String.dropLeft 4 line :: codeAcc) resultAcc

            else
                let
                    flushed =
                        flushIndentedCodeBlock codeAcc resultAcc
                in
                groupIndentedHelp rest [] (("    " ++ line) :: flushed)


flushIndentedCodeBlock : List String -> List String -> List String
flushIndentedCodeBlock codeAcc resultAcc =
    case codeAcc of
        [] ->
            resultAcc

        _ ->
            let
                codeText =
                    codeAcc |> List.reverse |> String.join "\n"

                highlighted =
                    highlightElm "      " codeText
            in
            highlighted :: resultAcc



-- SYNTAX HIGHLIGHTING


consoleOptions : SyntaxHighlight.ConsoleOptions
consoleOptions =
    { default = identity
    , highlight = identity
    , addition = identity
    , deletion = identity
    , comment = Ansi.Color.fontColor Ansi.Color.brightBlack
    , style1 = Ansi.Color.fontColor Ansi.Color.magenta -- numbers
    , style2 = Ansi.Color.fontColor Ansi.Color.green -- strings
    , style3 = Ansi.Color.fontColor Ansi.Color.magenta -- keywords, operators
    , style4 = Ansi.Color.fontColor Ansi.Color.cyan -- type signatures, group symbols
    , style5 = Ansi.Color.fontColor Ansi.Color.blue -- functions
    , style6 = Ansi.Color.fontColor Ansi.Color.yellow -- capitalized types
    , style7 = identity -- parameters
    }


{-| Highlight a code string as Elm with ANSI colors.
Each line is prefixed with the given indent string.
Falls back to dim/faint rendering if parsing fails.
-}
highlightElm : String -> String -> String
highlightElm indent code =
    case SyntaxHighlight.elm code of
        Ok hcode ->
            hcode
                |> SyntaxHighlight.toConsole consoleOptions
                |> List.map
                    (\line ->
                        if String.isEmpty (String.trim line) then
                            ""

                        else
                            indent ++ line
                    )
                |> String.join "\n"

        Err _ ->
            -- Fallback: just dim the code
            code
                |> String.lines
                |> List.map
                    (\line ->
                        if String.isEmpty (String.trim line) then
                            ""

                        else
                            indent ++ Ansi.Font.faint line
                    )
                |> String.join "\n"
