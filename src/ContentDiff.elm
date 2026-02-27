module ContentDiff exposing
    ( ContentDiff, ModuleContentDiff, ItemContentDiff
    , DiffLine, DiffLineStatus(..)
    , decoder, lookupItem, getReadmeDiff
    )

{-|

@docs ContentDiff, ModuleContentDiff, ItemContentDiff
@docs DiffLine, DiffLineStatus
@docs decoder, lookupItem, getReadmeDiff

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias ContentDiff =
    { modules : Dict String ModuleContentDiff
    , readmeDiff : Maybe (List DiffLine)
    }


type alias ModuleContentDiff =
    { items : Dict String ItemContentDiff }


type alias ItemContentDiff =
    { commentDiff : Maybe (List DiffLine)
    , oldAnnotation : Maybe String
    }


type alias DiffLine =
    { status : DiffLineStatus
    , content : String
    }


type DiffLineStatus
    = DiffContext
    | DiffAdded
    | DiffRemoved


lookupItem : ContentDiff -> String -> String -> Maybe ItemContentDiff
lookupItem contentDiff moduleName itemName =
    Dict.get moduleName contentDiff.modules
        |> Maybe.andThen (\moduleDiff -> Dict.get itemName moduleDiff.items)


getReadmeDiff : ContentDiff -> Maybe (List DiffLine)
getReadmeDiff contentDiff =
    contentDiff.readmeDiff


decoder : Decoder (Maybe ContentDiff)
decoder =
    Decode.nullable contentDiffDecoder


contentDiffDecoder : Decoder ContentDiff
contentDiffDecoder =
    Decode.map2 ContentDiff
        (Decode.field "modules" (Decode.dict moduleContentDiffDecoder))
        (Decode.maybe (Decode.field "readmeDiff" (Decode.list diffLineDecoder)))


moduleContentDiffDecoder : Decoder ModuleContentDiff
moduleContentDiffDecoder =
    Decode.map ModuleContentDiff
        (Decode.field "items" (Decode.dict itemContentDiffDecoder))


itemContentDiffDecoder : Decoder ItemContentDiff
itemContentDiffDecoder =
    Decode.map2 ItemContentDiff
        (Decode.maybe (Decode.field "commentDiff" (Decode.list diffLineDecoder)))
        (Decode.maybe (Decode.field "oldAnnotation" Decode.string))


diffLineDecoder : Decoder DiffLine
diffLineDecoder =
    Decode.map2 DiffLine
        (Decode.index 0 diffLineStatusDecoder)
        (Decode.index 1 Decode.string)


diffLineStatusDecoder : Decoder DiffLineStatus
diffLineStatusDecoder =
    Decode.int
        |> Decode.andThen
            (\status ->
                if status == -1 then
                    Decode.succeed DiffRemoved

                else if status == 0 then
                    Decode.succeed DiffContext

                else if status == 1 then
                    Decode.succeed DiffAdded

                else
                    Decode.fail ("Unknown diff status: " ++ String.fromInt status)
            )
