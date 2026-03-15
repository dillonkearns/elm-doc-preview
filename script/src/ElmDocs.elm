module ElmDocs exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.File
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
    | DiffRefs String String


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
            case String.split ".." value of
                [ base, head ] ->
                    DiffRefs base head

                _ ->
                    DiffPublished


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
    let
        depsObj =
            elmJson.directDeps
                |> List.map (\( name, version ) -> ( name, Encode.string version ))
                |> Encode.object

        input =
            Encode.object
                [ ( "sourceDirectories", Encode.list Encode.string elmJson.sourceDirectories )
                , ( "directDeps", depsObj )
                , ( "projectDir", Encode.string "." )
                ]
    in
    BackendTask.Custom.run "buildApplicationDocs"
        input
        Decode.string
        |> BackendTask.quiet
        |> BackendTask.allowFatal
        |> BackendTask.andThen (decodeAndRender options elmJson)


decodeAndRender : CliOptions -> ElmJson -> String -> BackendTask FatalError ()
decodeAndRender options elmJson jsonString =
    case Decode.decodeString (Decode.list Docs.decoder) jsonString of
        Ok modules ->
            case parseDiffMode options.diff of
                NoDiff ->
                    renderOutput options Nothing modules

                DiffPublished ->
                    case ( elmJson.packageName, elmJson.packageVersion ) of
                        ( Just name, Just version ) ->
                            computeDiff
                                (Encode.object
                                    [ ( "mode", Encode.string "published" )
                                    , ( "headDocs", Encode.string jsonString )
                                    , ( "packageName", Encode.string name )
                                    , ( "version", Encode.string version )
                                    ]
                                )
                                |> BackendTask.andThen
                                    (\maybeDiff ->
                                        renderOutput options maybeDiff modules
                                    )

                        _ ->
                            BackendTask.fail
                                (FatalError.fromString
                                    "--diff without refs only works for packages with a published version. Use --diff base..head for applications."
                                )

                DiffRefs base head ->
                    let
                        depsObj =
                            elmJson.directDeps
                                |> List.map (\( n, v ) -> ( n, Encode.string v ))
                                |> Encode.object
                    in
                    computeDiff
                        (Encode.object
                            [ ( "mode", Encode.string "refs" )
                            , ( "headDocs", Encode.string jsonString )
                            , ( "base", Encode.string base )
                            , ( "sourceDirectories", Encode.list Encode.string elmJson.sourceDirectories )
                            , ( "directDeps", depsObj )
                            , ( "projectDir", Encode.string "." )
                            ]
                        )
                        |> BackendTask.andThen
                            (\maybeDiff ->
                                renderOutput options maybeDiff modules
                            )

        Err err ->
            BackendTask.fail
                (FatalError.fromString
                    ("Failed to decode docs.json: " ++ Decode.errorToString err)
                )


computeDiff : Encode.Value -> BackendTask FatalError (Maybe ApiDiff)
computeDiff input =
    BackendTask.Custom.run "computeApiDiff"
        input
        Diff.decoder
        |> BackendTask.quiet
        |> BackendTask.allowFatal


renderOutput : CliOptions -> Maybe ApiDiff -> List Docs.Module -> BackendTask FatalError ()
renderOutput options maybeDiff modules =
    case ( options.moduleName, maybeDiff ) of
        ( Nothing, Nothing ) ->
            Script.log (Render.renderToc modules)

        ( Nothing, Just diff ) ->
            Script.log (Render.renderTocWithDiff diff modules)

        ( Just name, Nothing ) ->
            case findModule name modules of
                Just module_ ->
                    Script.log (Render.renderModule module_)

                Nothing ->
                    moduleNotFound name modules

        ( Just name, Just diff ) ->
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
