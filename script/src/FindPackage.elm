module FindPackage exposing (FindResult, findPackageElmJson)

{-| Walk up parent directories to find the nearest `elm.json` with
`"type": "package"`. Falls back to the nearest `elm.json` of any type.
-}

import BackendTask exposing (BackendTask)
import BackendTask.File
import FatalError exposing (FatalError)
import Json.Decode as Decode


type alias FindResult =
    { projectType : String
    , dir : String
    }


findPackageElmJson : String -> BackendTask FatalError FindResult
findPackageElmJson startDir =
    findPackageElmJsonHelper (parentDirs startDir) Nothing


parentDirs : String -> List String
parentDirs startDir =
    List.range 0 5
        |> List.map (\n -> dropSegments n startDir)
        |> dedupStrings


dropSegments : Int -> String -> String
dropSegments n path =
    let
        segments =
            path
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
    in
    case List.take (max 1 (List.length segments - n)) segments of
        [] ->
            "/"

        parts ->
            "/" ++ String.join "/" parts


dedupStrings : List String -> List String
dedupStrings items =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        items


findPackageElmJsonHelper : List String -> Maybe FindResult -> BackendTask FatalError FindResult
findPackageElmJsonHelper dirs fallback =
    case dirs of
        [] ->
            case fallback of
                Just fb ->
                    BackendTask.succeed fb

                Nothing ->
                    BackendTask.fail
                        (FatalError.fromString "No elm.json found in current or parent directories.")

        dir :: rest ->
            BackendTask.File.rawFile (dir ++ "/elm.json")
                |> BackendTask.map Just
                |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)
                |> BackendTask.andThen
                    (\maybeContents ->
                        case maybeContents of
                            Just contents ->
                                case Decode.decodeString projectTypeDecoder contents of
                                    Ok projectType ->
                                        if projectType == "package" then
                                            BackendTask.succeed { projectType = projectType, dir = dir }

                                        else
                                            let
                                                newFallback =
                                                    case fallback of
                                                        Nothing ->
                                                            Just { projectType = projectType, dir = dir }

                                                        Just _ ->
                                                            fallback
                                            in
                                            findPackageElmJsonHelper rest newFallback

                                    Err _ ->
                                        findPackageElmJsonHelper rest fallback

                            Nothing ->
                                findPackageElmJsonHelper rest fallback
                    )


projectTypeDecoder : Decode.Decoder String
projectTypeDecoder =
    Decode.field "type" Decode.string
