module DiffTuiApp exposing (run)

{-| Entry point for the lazygit-style TUI diff viewer.

    elm-pages run script/src/DiffTuiApp.elm
    elm-pages run script/src/DiffTuiApp.elm -- --diff HEAD
    elm-pages run script/src/DiffTuiApp.elm -- --diff main..feature

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.File
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DiffTui
import Docs.Diff as Diff exposing (ApiDiff)
import Elm.Docs as Docs
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
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
                        |> Option.withDescription "Diff mode: omit for vs published, or base..head for git refs"
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
                loadDiffWithModules options
                    |> BackendTask.andThen
                        (\result ->
                            case result.diff of
                                Just diff ->
                                    BackendTask.succeed { diff = diff, modules = result.modules }

                                Nothing ->
                                    BackendTask.fail
                                        (FatalError.fromString "No API changes detected.")
                        )
            , init = DiffTui.init
            , update = DiffTui.update
            , view = DiffTui.view
            , subscriptions = DiffTui.subscriptions
            }
        )


type alias ElmJson =
    { projectType : String
    , sourceDirectories : List String
    , directDeps : List ( String, String )
    , packageName : Maybe String
    , packageVersion : Maybe String
    }


loadDiffWithModules : CliOptions -> BackendTask FatalError { diff : Maybe ApiDiff, modules : List Docs.Module }
loadDiffWithModules options =
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
                            in
                            (case parseDiffMode options.diff of
                                DiffPublished ->
                                    fetchPublishedDiff elmJson headDocsJson

                                DiffRefs base ->
                                    buildRefDiff base elmJson headDocsJson
                            )
                                |> BackendTask.map
                                    (\maybeDiff ->
                                        { diff = maybeDiff, modules = modulesResult }
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
                    "--diff without refs only works for packages with a published version."
                )


buildRefDiff : String -> ElmJson -> String -> BackendTask FatalError (Maybe ApiDiff)
buildRefDiff ref elmJson headDocsJson =
    createWorktree ref
        |> BackendTask.andThen
            (\tempDir ->
                buildDocsInWorktree tempDir elmJson
                    |> BackendTask.andThen
                        (\baseDocs ->
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
