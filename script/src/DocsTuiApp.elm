module DocsTuiApp exposing (run)

{-| Entry point for the unified documentation TUI.

    elm-pages run script/src/DocsTuiApp.elm
    elm-pages run script/src/DocsTuiApp.elm -- --diff HEAD
    elm-pages run script/src/DocsTuiApp.elm -- --diff main..feature

Without --diff: browse mode (all modules, full docs).
With --diff: browse + diff mode (d toggles diff view, [2] Changes tab).

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Env
import BackendTask.File
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DocsTui
import Docs.Diff as Diff exposing (ApiDiff)
import Elm.Docs as Docs
import FatalError exposing (FatalError)
import FindPackage
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Regex
import Tui.Effect as Effect


type alias CliOptions =
    { diff : Maybe String
    }


type DiffMode
    = DiffPublished
    | DiffRefs String


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "diff"
                        |> Option.withDescription "Diff mode: omit for browse-only, or specify ref/range for diff"
                    )
            )


parseDiffMode : Maybe String -> DiffMode
parseDiffMode maybeDiff =
    case maybeDiff of
        Nothing ->
            DiffPublished

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
                DiffRefs value


run : Script
run =
    Script.tuiWithCliOptions program
        (\options ->
            { data =
                loadDocsWithOptionalDiff options
            , init = DocsTui.init
            , update = DocsTui.update
            , view = DocsTui.view
            , subscriptions = DocsTui.subscriptions
            }
        )


type alias ElmJson =
    { projectType : String
    , sourceDirectories : List String
    , directDeps : List ( String, String )
    , packageName : Maybe String
    , packageVersion : Maybe String
    }


loadDocsWithOptionalDiff :
    CliOptions
    ->
        BackendTask
            FatalError
            { modules : List Docs.Module
            , diff : Maybe ApiDiff
            , versions : List String
            , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
            , dependencies : List String
            , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
            }
loadDocsWithOptionalDiff options =
    Script.command "pwd" []
        |> BackendTask.map String.trim
        |> BackendTask.andThen
            (\cwd ->
                FindPackage.findPackageElmJson cwd
                    |> BackendTask.andThen
                        (\found ->
                            BackendTask.inDir found.dir
                                (loadDocsInDir options)
                        )
            )


loadDocsInDir :
    CliOptions
    ->
        BackendTask
            FatalError
            { modules : List Docs.Module
            , diff : Maybe ApiDiff
            , versions : List String
            , loadDiff : String -> BackendTask FatalError (Maybe ApiDiff)
            , dependencies : List String
            , loadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
            }
loadDocsInDir options =
    readElmJson
        |> BackendTask.andThen
            (\elmJson ->
                buildCurrentDocs elmJson
                    |> BackendTask.andThen
                        (\headDocsJson ->
                            let
                                modulesResult =
                                    case Decode.decodeString (Decode.list Docs.decoder) headDocsJson of
                                        Ok modules ->
                                            modules

                                        Err _ ->
                                            []

                                loadDiffFn =
                                    buildLoadDiff elmJson headDocsJson
                            in
                            loadVersions
                                |> BackendTask.andThen
                                    (\versions ->
                                        case options.diff of
                                            Nothing ->
                                                -- Browse-only mode: no diff
                                                BackendTask.succeed
                                                    { modules = modulesResult
                                                    , diff = Nothing
                                                    , versions = versions
                                                    , loadDiff = loadDiffFn
                                                    , dependencies = List.map Tuple.first elmJson.directDeps
                                                    , loadPackageDocs = buildLoadPackageDocs
                                                    }

                                            Just _ ->
                                                -- Diff mode: compute diff
                                                (case parseDiffMode options.diff of
                                                    DiffPublished ->
                                                        fetchPublishedDiff elmJson headDocsJson

                                                    DiffRefs base ->
                                                        buildRefDiff base elmJson headDocsJson
                                                )
                                                    |> BackendTask.map
                                                        (\maybeDiff ->
                                                            { modules = modulesResult
                                                            , diff = maybeDiff
                                                            , versions = versions
                                                            , loadDiff = loadDiffFn
                                                            , dependencies = List.map Tuple.first elmJson.directDeps
                                                            , loadPackageDocs = buildLoadPackageDocs
                                                            }
                                                        )
                                    )
                        )
            )


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
                            (FatalError.fromString "Could not read elm.json.")
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


buildCurrentDocs : ElmJson -> BackendTask FatalError String
buildCurrentDocs elmJson =
    resolveRefToSha "HEAD"
        |> BackendTask.andThen
            (\headSha ->
                hasUncommittedChanges
                    |> BackendTask.andThen
                        (\isDirty ->
                            if isDirty then
                                -- Working tree is dirty, don't use cache
                                buildCurrentDocsUncached elmJson

                            else
                                readCachedDocs headSha
                                    |> BackendTask.andThen
                                        (\maybeCached ->
                                            case maybeCached of
                                                Just cachedDocs ->
                                                    BackendTask.succeed cachedDocs

                                                Nothing ->
                                                    buildCurrentDocsUncached elmJson
                                                        |> BackendTask.andThen
                                                            (\docs ->
                                                                writeCachedDocs headSha docs
                                                                    |> BackendTask.map (\() -> docs)
                                                            )
                                        )
                        )
            )


buildCurrentDocsUncached : ElmJson -> BackendTask FatalError String
buildCurrentDocsUncached elmJson =
    if elmJson.projectType == "package" then
        Script.exec "elm" [ "make", "--docs=docs.json" ]
            |> BackendTask.quiet
            |> BackendTask.andThen
                (\() ->
                    BackendTask.File.rawFile "docs.json"
                        |> BackendTask.allowFatal
                )

    else
        BackendTask.Custom.run "buildApplicationDocs"
            (Encode.object
                [ ( "sourceDirectories", Encode.list Encode.string elmJson.sourceDirectories )
                , ( "directDeps"
                  , elmJson.directDeps
                        |> List.map (\( n, v ) -> ( n, Encode.string v ))
                        |> Encode.object
                  )
                , ( "projectDir", Encode.string "." )
                ]
            )
            Decode.string
            |> BackendTask.quiet
            |> BackendTask.allowFatal


fetchPublishedDiff : ElmJson -> String -> BackendTask FatalError (Maybe ApiDiff)
fetchPublishedDiff elmJson headDocsJson =
    case ( elmJson.packageName, elmJson.packageVersion ) of
        ( Just name, Just version ) ->
            readDocsFromElmHome name version
                |> BackendTask.andThen
                    (\maybeCachedDocs ->
                        case maybeCachedDocs of
                            Just baseDocsJson ->
                                computeDiff baseDocsJson headDocsJson

                            Nothing ->
                                fetchAndCacheDocs name version
                                    |> BackendTask.andThen
                                        (\baseDocsJson -> computeDiff baseDocsJson headDocsJson)
                    )

        _ ->
            BackendTask.fail
                (FatalError.fromString
                    "--diff without refs only works for packages with a published version."
                )


readDocsFromElmHome : String -> String -> BackendTask FatalError (Maybe String)
readDocsFromElmHome packageName version =
    resolveElmHome
        |> BackendTask.andThen
            (\elmHome ->
                let
                    docsPath =
                        elmHome ++ "/0.19.1/packages/" ++ packageName ++ "/" ++ version ++ "/docs.json"
                in
                BackendTask.File.rawFile docsPath
                    |> BackendTask.map Just
                    |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)
            )


resolveElmHome : BackendTask FatalError String
resolveElmHome =
    BackendTask.Env.get "ELM_HOME"
        |> BackendTask.andThen
            (\maybeElmHome ->
                case maybeElmHome of
                    Just elmHome ->
                        BackendTask.succeed elmHome

                    Nothing ->
                        BackendTask.Env.get "HOME"
                            |> BackendTask.map
                                (\maybeHome ->
                                    case maybeHome of
                                        Just home ->
                                            home ++ "/.elm"

                                        Nothing ->
                                            "/Users/" ++ "unknown" ++ "/.elm"
                                )
            )


fetchAndCacheDocs : String -> String -> BackendTask FatalError String
fetchAndCacheDocs packageName version =
    let
        url =
            "https://package.elm-lang.org/packages/"
                ++ packageName
                ++ "/"
                ++ version
                ++ "/docs.json"
    in
    BackendTask.Http.get url BackendTask.Http.expectString
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\docsJson ->
                writeDocsToElmHome packageName version docsJson
                    |> BackendTask.map (\() -> docsJson)
            )


writeDocsToElmHome : String -> String -> String -> BackendTask FatalError ()
writeDocsToElmHome packageName version docsJson =
    resolveElmHome
        |> BackendTask.andThen
            (\elmHome ->
                let
                    dirPath =
                        elmHome ++ "/0.19.1/packages/" ++ packageName ++ "/" ++ version

                    docsPath =
                        dirPath ++ "/docs.json"
                in
                -- Only write if the directory already exists (package is known to Elm)
                BackendTask.File.exists dirPath
                    |> BackendTask.andThen
                        (\dirExists ->
                            if dirExists then
                                BackendTask.Custom.run "writeFileAt"
                                    (Encode.object
                                        [ ( "path", Encode.string docsPath )
                                        , ( "content", Encode.string docsJson )
                                        ]
                                    )
                                    (Decode.null ())
                                    |> BackendTask.allowFatal

                            else
                                BackendTask.succeed ()
                        )
            )


buildRefDiff : String -> ElmJson -> String -> BackendTask FatalError (Maybe ApiDiff)
buildRefDiff ref elmJson headDocsJson =
    resolveRefToSha ref
        |> BackendTask.andThen
            (\sha ->
                readCachedDocs sha
                    |> BackendTask.andThen
                        (\maybeCachedDocs ->
                            case maybeCachedDocs of
                                Just cachedDocs ->
                                    -- Cache hit: skip worktree entirely
                                    readOptionalFile "README.md"
                                        |> BackendTask.andThen
                                            (\headReadme ->
                                                computeDiffWithReadme cachedDocs headDocsJson Nothing headReadme
                                            )

                                Nothing ->
                                    -- Cache miss: build via worktree, then cache
                                    buildRefDiffUncached ref sha elmJson headDocsJson
                        )
            )


buildRefDiffUncached : String -> String -> ElmJson -> String -> BackendTask FatalError (Maybe ApiDiff)
buildRefDiffUncached ref sha elmJson headDocsJson =
    createWorktree ref
        |> BackendTask.andThen
            (\tempDir ->
                buildDocsInWorktree tempDir elmJson
                    |> BackendTask.andThen
                        (\baseDocs ->
                            writeCachedDocs sha baseDocs
                                |> BackendTask.andThen
                                    (\() ->
                                        readOptionalFile (tempDir ++ "/README.md")
                                            |> BackendTask.andThen
                                                (\baseReadme ->
                                                    readOptionalFile "README.md"
                                                        |> BackendTask.andThen
                                                            (\headReadme ->
                                                                cleanupWorktree tempDir
                                                                    |> BackendTask.andThen
                                                                        (\() ->
                                                                            computeDiffWithReadme baseDocs headDocsJson baseReadme headReadme
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


resolveRefToSha : String -> BackendTask FatalError String
resolveRefToSha ref =
    Script.command "git" [ "rev-parse", ref ]
        |> BackendTask.map String.trim


hasUncommittedChanges : BackendTask FatalError Bool
hasUncommittedChanges =
    Script.command "git" [ "status", "--porcelain" ]
        |> BackendTask.map (\output -> String.trim output /= "")


readCachedDocs : String -> BackendTask FatalError (Maybe String)
readCachedDocs sha =
    BackendTask.Custom.run "readCachedDocs"
        (Encode.string sha)
        (Decode.nullable Decode.string)
        |> BackendTask.allowFatal


writeCachedDocs : String -> String -> BackendTask FatalError ()
writeCachedDocs sha docs =
    BackendTask.Custom.run "writeCachedDocs"
        (Encode.object
            [ ( "sha", Encode.string sha )
            , ( "docs", Encode.string docs )
            ]
        )
        (Decode.null ())
        |> BackendTask.allowFatal


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
                            Script.exec "bash" [ "-c", "cd '" ++ tempDir ++ "' && elm make --docs=docs.json" ]
                                |> BackendTask.quiet
                                |> BackendTask.andThen
                                    (\() ->
                                        BackendTask.File.rawFile (tempDir ++ "/docs.json")
                                            |> BackendTask.allowFatal
                                    )

                        else
                            BackendTask.Custom.run "buildApplicationDocs"
                                (Encode.object
                                    [ ( "sourceDirectories", Encode.list Encode.string worktreeElmJson.sourceDirectories )
                                    , ( "directDeps"
                                      , worktreeElmJson.directDeps
                                            |> List.map (\( n, v ) -> ( n, Encode.string v ))
                                            |> Encode.object
                                      )
                                    , ( "projectDir", Encode.string tempDir )
                                    ]
                                )
                                Decode.string
                                |> BackendTask.quiet
                                |> BackendTask.allowFatal

                    Err _ ->
                        BackendTask.fail
                            (FatalError.fromString "Could not parse elm.json in worktree.")
            )


cleanupWorktree : String -> BackendTask FatalError ()
cleanupWorktree tempDir =
    Script.exec "git" [ "worktree", "remove", "--force", tempDir ]
        |> BackendTask.quiet


readOptionalFile : String -> BackendTask FatalError (Maybe String)
readOptionalFile path =
    BackendTask.File.rawFile path
        |> BackendTask.map Just
        |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)


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


loadVersions : BackendTask FatalError (List String)
loadVersions =
    Script.command "git" [ "tag", "-l" ]
        |> BackendTask.map parseVersionTags


parseVersionTags : String -> List String
parseVersionTags output =
    let
        versionRegex =
            Regex.fromString "^\\d+\\.\\d+\\.\\d+$"
                |> Maybe.withDefault Regex.never
    in
    output
        |> String.lines
        |> List.filter (\line -> Regex.contains versionRegex (String.trim line))
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.sortWith reverseVersionCompare


reverseVersionCompare : String -> String -> Order
reverseVersionCompare a b =
    let
        toInts s =
            s
                |> String.split "."
                |> List.filterMap String.toInt
    in
    compareLists (toInts b) (toInts a)


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


buildLoadDiff : ElmJson -> String -> (String -> BackendTask FatalError (Maybe ApiDiff))
buildLoadDiff elmJson headDocsJson =
    \version ->
        case elmJson.packageName of
            Just packageName ->
                -- Try ELM_HOME first for published versions
                readDocsFromElmHome packageName version
                    |> BackendTask.andThen
                        (\maybeCachedDocs ->
                            case maybeCachedDocs of
                                Just baseDocsJson ->
                                    computeDiff baseDocsJson headDocsJson

                                Nothing ->
                                    buildRefDiff version elmJson headDocsJson
                        )

            Nothing ->
                buildRefDiff version elmJson headDocsJson


buildLoadPackageDocs : String -> BackendTask FatalError (List Docs.Module)
buildLoadPackageDocs packageName =
    -- Try ELM_HOME first, then fetch from registry
    resolveElmHome
        |> BackendTask.andThen
            (\elmHome ->
                let
                    packageDir =
                        elmHome ++ "/0.19.1/packages/" ++ packageName
                in
                BackendTask.File.exists packageDir
                    |> BackendTask.andThen
                        (\dirExists ->
                            if not dirExists then
                                fetchPackageDocsFromRegistry packageName

                            else
                                -- Find versions by listing directory
                                Script.command "ls" [ packageDir ]
                                    |> BackendTask.map
                                        (\output ->
                                            output
                                                |> String.trim
                                                |> String.lines
                                                |> List.filter (not << String.isEmpty)
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault ""
                                        )
                                    |> BackendTask.onError (\_ -> BackendTask.succeed "")
                                    |> BackendTask.andThen
                                        (\latestVersion ->
                                            if String.isEmpty latestVersion then
                                                fetchPackageDocsFromRegistry packageName

                                            else
                                                let
                                                    docsPath =
                                                        packageDir ++ "/" ++ latestVersion ++ "/docs.json"
                                                in
                                                BackendTask.File.rawFile docsPath
                                                    |> BackendTask.map
                                                        (\json ->
                                                            case Decode.decodeString (Decode.list Docs.decoder) json of
                                                                Ok modules ->
                                                                    modules

                                                                Err _ ->
                                                                    []
                                                        )
                                                    |> BackendTask.onError
                                                        (\_ -> fetchPackageDocsFromRegistry packageName)
                                        )
                        )
            )


fetchPackageDocsFromRegistry : String -> BackendTask FatalError (List Docs.Module)
fetchPackageDocsFromRegistry packageName =
    let
        url =
            "https://package.elm-lang.org/packages/"
                ++ packageName
                ++ "/latest/docs.json"
    in
    BackendTask.Http.get url BackendTask.Http.expectString
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\json ->
                case Decode.decodeString (Decode.list Docs.decoder) json of
                    Ok modules ->
                        BackendTask.succeed modules

                    Err _ ->
                        BackendTask.succeed []
            )
