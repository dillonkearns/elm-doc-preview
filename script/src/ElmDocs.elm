module ElmDocs exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Docs.Render as Render
import Elm.Docs as Docs
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


type alias CliOptions =
    { moduleName : Maybe String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "module"
                        |> Option.withDescription "Show docs for a specific module"
                    )
            )


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
    Decode.map3 ElmJson
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


buildDocs : CliOptions -> ElmJson -> BackendTask FatalError ()
buildDocs options elmJson =
    case elmJson.projectType of
        "package" ->
            Do.exec "elm" [ "make", "--docs=docs.json" ]
                (\() ->
                    BackendTask.File.rawFile "docs.json"
                        |> BackendTask.allowFatal
                        |> BackendTask.andThen (decodeAndRender options)
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
        |> BackendTask.allowFatal
        |> BackendTask.andThen (decodeAndRender options)


decodeAndRender : CliOptions -> String -> BackendTask FatalError ()
decodeAndRender options jsonString =
    case Decode.decodeString (Decode.list Docs.decoder) jsonString of
        Ok modules ->
            renderOutput options modules

        Err err ->
            BackendTask.fail
                (FatalError.fromString
                    ("Failed to decode docs.json: " ++ Decode.errorToString err)
                )


renderOutput : CliOptions -> List Docs.Module -> BackendTask FatalError ()
renderOutput options modules =
    case options.moduleName of
        Nothing ->
            Script.log (Render.renderToc modules)

        Just name ->
            case findModule name modules of
                Just module_ ->
                    Script.log (Render.renderModule module_)

                Nothing ->
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
