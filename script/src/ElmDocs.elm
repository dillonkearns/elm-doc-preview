module ElmDocs exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.File
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Docs.Diff as Diff exposing (ApiDiff)
import Docs.Render as Render
import Elm.Docs as Docs
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


type alias CliOptions =
    { moduleName : Maybe String
    , diff : Maybe String
    }


type DiffMode
    = NoDiff
    | DiffPublished
    | DiffRefs String


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "module"
                        |> Option.withDescription "Show docs for a specific module"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "diff"
                        |> Option.withDescription "Show API diff (omit value for vs published, or base..head for git refs)"
                    )
            )


parseDiffMode : Maybe String -> DiffMode
parseDiffMode maybeDiff =
    case maybeDiff of
        Nothing ->
            NoDiff

        Just value ->
            if String.contains ".." value then
                case String.split ".." value of
                    [ base, _ ] ->
                        DiffRefs base

                    _ ->
                        DiffPublished

            else if value == "published" then
                DiffPublished

            else
                -- Single ref like HEAD, HEAD~1, main, etc.
                DiffRefs value


run : Script
run =
    Script.withCliOptions program
        (\options ->
            readElmJson
                |> BackendTask.andThen (buildDocs options)
        )


type alias ElmJson =
    { projectType : String
    , sourceDirectories : List String
    , directDeps : List ( String, String )
    , packageName : Maybe String
    , packageVersion : Maybe String
    }


readElmJson : BackendTask FatalError ElmJson
readElmJson =
    BackendTask.File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\contents ->
                case Decode.decodeString elmJsonDecoder contents of
                    Ok elmJson ->
                        BackendTask.succeed elmJson

                    Err _ ->
                        BackendTask.fail
                            (FatalError.fromString "Could not read elm.json. Make sure you're in an Elm project directory.")
            )


elmJsonDecoder : Decode.Decoder ElmJson
elmJsonDecoder =
    Decode.map5 ElmJson
        (Decode.field "type" Decode.string)
        (Decode.oneOf
            [ Decode.field "source-directories" (Decode.list Decode.string)
            , Decode.succeed [ "src" ]
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "dependencies", "direct" ] (Decode.keyValuePairs Decode.string)
            , Decode.field "dependencies" (Decode.keyValuePairs Decode.string)
            , Decode.succeed []
            ]
        )
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.maybe (Decode.field "version" Decode.string))


buildDocs : CliOptions -> ElmJson -> BackendTask FatalError ()
buildDocs options elmJson =
    case elmJson.projectType of
        "package" ->
            Script.exec "elm" [ "make", "--docs=docs.json" ]
                |> BackendTask.quiet
                |> BackendTask.andThen
                    (\() ->
                        BackendTask.File.rawFile "docs.json"
                            |> BackendTask.allowFatal
                            |> BackendTask.andThen (decodeAndRender options elmJson)
                    )

        _ ->
            buildApplicationDocs options elmJson


buildApplicationDocs : CliOptions -> ElmJson -> BackendTask FatalError ()
buildApplicationDocs options elmJson =
    BackendTask.Custom.run "buildApplicationDocs"
        (encodeAppDocsInput elmJson ".")
        Decode.string
        |> BackendTask.quiet
        |> BackendTask.allowFatal
        |> BackendTask.andThen (decodeAndRender options elmJson)


encodeAppDocsInput : ElmJson -> String -> Encode.Value
encodeAppDocsInput elmJson projectDir =
    let
        depsObj =
            elmJson.directDeps
                |> List.map (\( name, version ) -> ( name, Encode.string version ))
                |> Encode.object
    in
    Encode.object
        [ ( "sourceDirectories", Encode.list Encode.string elmJson.sourceDirectories )
        , ( "directDeps", depsObj )
        , ( "projectDir", Encode.string projectDir )
        ]


decodeAndRender : CliOptions -> ElmJson -> String -> BackendTask FatalError ()
decodeAndRender options elmJson jsonString =
    case Decode.decodeString (Decode.list Docs.decoder) jsonString of
        Ok modules ->
            case parseDiffMode options.diff of
                NoDiff ->
                    renderOutput options Nothing modules

                DiffPublished ->
                    fetchPublishedDiff elmJson jsonString
                        |> BackendTask.andThen
                            (\maybeDiff -> renderOutput options maybeDiff modules)

                DiffRefs base ->
                    buildBaseDocs base elmJson
                        |> BackendTask.andThen
                            (\{ baseDocs, baseReadme } ->
                                readOptionalFile "README.md"
                                    |> BackendTask.andThen
                                        (\headReadme ->
                                            computeDiffWithReadme baseDocs jsonString baseReadme headReadme
                                                |> BackendTask.andThen
                                                    (\maybeDiff -> renderOutput options maybeDiff modules)
                                        )
                            )

        Err err ->
            BackendTask.fail
                (FatalError.fromString
                    ("Failed to decode docs.json: " ++ Decode.errorToString err)
                )



-- DIFF: PUBLISHED MODE


fetchPublishedDiff : ElmJson -> String -> BackendTask FatalError (Maybe ApiDiff)
fetchPublishedDiff elmJson headDocsJson =
    case ( elmJson.packageName, elmJson.packageVersion ) of
        ( Just name, Just version ) ->
            let
                url =
                    "https://package.elm-lang.org/packages/"
                        ++ name
                        ++ "/"
                        ++ version
                        ++ "/docs.json"
            in
            BackendTask.Http.get url BackendTask.Http.expectString
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\baseDocsJson -> computeDiff baseDocsJson headDocsJson)

        _ ->
            BackendTask.fail
                (FatalError.fromString
                    "--diff without refs only works for packages with a published version. Use --diff base..head for applications."
                )



-- DIFF: REFS MODE


buildBaseDocs : String -> ElmJson -> BackendTask FatalError { baseDocs : String, baseReadme : Maybe String }
buildBaseDocs ref elmJson =
    createWorktree ref
        |> BackendTask.andThen
            (\tempDir ->
                buildDocsInWorktree tempDir elmJson
                    |> BackendTask.andThen
                        (\baseDocs ->
                            readOptionalFile (tempDir ++ "/README.md")
                                |> BackendTask.andThen
                                    (\baseReadme ->
                                        cleanupWorktree tempDir
                                            |> BackendTask.map (\() -> { baseDocs = baseDocs, baseReadme = baseReadme })
                                    )
                        )
            )


createWorktree : String -> BackendTask FatalError String
createWorktree ref =
    Script.command "mktemp" [ "-d" ]
        |> BackendTask.map String.trim
        |> BackendTask.andThen
            (\tempDir ->
                Script.exec "rm" [ "-rf", tempDir ]
                    |> BackendTask.quiet
                    |> BackendTask.andThen
                        (\() ->
                            Script.exec "git" [ "worktree", "add", "--detach", tempDir, ref ]
                                |> BackendTask.quiet
                                |> BackendTask.map (\() -> tempDir)
                        )
            )


buildDocsInWorktree : String -> ElmJson -> BackendTask FatalError String
buildDocsInWorktree tempDir elmJson =
    BackendTask.File.rawFile (tempDir ++ "/elm.json")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\worktreeElmJsonStr ->
                case Decode.decodeString elmJsonDecoder worktreeElmJsonStr of
                    Ok worktreeElmJson ->
                        if worktreeElmJson.projectType == "package" then
                            buildPackageDocsInDir tempDir

                        else
                            BackendTask.Custom.run "buildApplicationDocs"
                                (encodeAppDocsInput worktreeElmJson tempDir)
                                Decode.string
                                |> BackendTask.quiet
                                |> BackendTask.allowFatal

                    Err _ ->
                        BackendTask.fail
                            (FatalError.fromString "Could not parse elm.json in worktree.")
            )


buildPackageDocsInDir : String -> BackendTask FatalError String
buildPackageDocsInDir dir =
    let
        docsPath =
            dir ++ "/docs.json"
    in
    Script.exec "bash" [ "-c", "cd '" ++ dir ++ "' && elm make --docs=docs.json" ]
        |> BackendTask.quiet
        |> BackendTask.andThen
            (\() ->
                BackendTask.File.rawFile docsPath
                    |> BackendTask.allowFatal
            )


cleanupWorktree : String -> BackendTask FatalError ()
cleanupWorktree tempDir =
    Script.exec "git" [ "worktree", "remove", "--force", tempDir ]
        |> BackendTask.quiet



-- HELPERS


readOptionalFile : String -> BackendTask FatalError (Maybe String)
readOptionalFile path =
    BackendTask.File.rawFile path
        |> BackendTask.map Just
        |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)



-- DIFF: COMPUTE


computeDiff : String -> String -> BackendTask FatalError (Maybe ApiDiff)
computeDiff baseDocsJson headDocsJson =
    computeDiffWithReadme baseDocsJson headDocsJson Nothing Nothing


computeDiffWithReadme : String -> String -> Maybe String -> Maybe String -> BackendTask FatalError (Maybe ApiDiff)
computeDiffWithReadme baseDocsJson headDocsJson baseReadme headReadme =
    let
        readmeFields =
            case ( baseReadme, headReadme ) of
                ( Just br, Just hr ) ->
                    [ ( "baseReadme", Encode.string br )
                    , ( "headReadme", Encode.string hr )
                    ]

                ( Just br, Nothing ) ->
                    [ ( "baseReadme", Encode.string br )
                    , ( "headReadme", Encode.string "" )
                    ]

                ( Nothing, Just hr ) ->
                    [ ( "baseReadme", Encode.string "" )
                    , ( "headReadme", Encode.string hr )
                    ]

                ( Nothing, Nothing ) ->
                    []
    in
    BackendTask.Custom.run "computeDiff"
        (Encode.object
            ([ ( "baseDocs", Encode.string baseDocsJson )
             , ( "headDocs", Encode.string headDocsJson )
             ]
                ++ readmeFields
            )
        )
        Diff.decoder
        |> BackendTask.quiet
        |> BackendTask.allowFatal



-- RENDERING


renderOutput : CliOptions -> Maybe ApiDiff -> List Docs.Module -> BackendTask FatalError ()
renderOutput options maybeDiff modules =
    let
        isDiffMode =
            parseDiffMode options.diff /= NoDiff
    in
    case ( options.moduleName, maybeDiff, isDiffMode ) of
        ( Nothing, Nothing, True ) ->
            Script.log "No API changes detected."

        ( Nothing, Nothing, False ) ->
            Script.log (Render.renderToc modules)

        ( Nothing, Just diff, _ ) ->
            Script.log (Render.renderFullDiff diff modules)

        ( Just name, Nothing, _ ) ->
            case findModule name modules of
                Just module_ ->
                    Script.log (Render.renderModule module_)

                Nothing ->
                    moduleNotFound name modules

        ( Just name, Just diff, _ ) ->
            case findModule name modules of
                Just module_ ->
                    Script.log (Render.renderModuleWithDiff diff module_)

                Nothing ->
                    moduleNotFound name modules


moduleNotFound : String -> List Docs.Module -> BackendTask FatalError ()
moduleNotFound name modules =
    let
        available =
            modules
                |> List.map .name
                |> String.join ", "
    in
    BackendTask.fail
        (FatalError.fromString
            ("Module '"
                ++ name
                ++ "' not found. Available modules: "
                ++ available
            )
        )


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
