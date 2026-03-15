module Docs.Diff exposing
    ( ApiDiff
    , DiffStatus(..)
    , ModuleChanges
    , ModuleStatus(..)
    , decoder
    , findModuleChanges
    , getCommentDiff
    , getModuleCommentDiff
    , hasCommentChanges
    , lookupItem
    , moduleStatus
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias ApiDiff =
    { magnitude : String
    , addedModules : List String
    , removedModules : List String
    , changedModules : List ModuleChanges
    , commentDiffs : Dict String (Dict String String)
    , readmeDiff : Maybe String
    }


type alias ModuleChanges =
    { name : String
    , added : List String
    , changed : List String
    , removed : List String
    }


type DiffStatus
    = Added
    | Changed
    | ModuleAdded
    | Unchanged


type ModuleStatus
    = ModuleNew
    | ModuleChanged
    | ModuleRemoved
    | ModuleUnchanged


decoder : Decoder (Maybe ApiDiff)
decoder =
    Decode.nullable apiDiffDecoder


apiDiffDecoder : Decoder ApiDiff
apiDiffDecoder =
    map6 ApiDiff
        (Decode.field "magnitude" Decode.string)
        (Decode.field "addedModules" (Decode.list Decode.string))
        (Decode.field "removedModules" (Decode.list Decode.string))
        (Decode.field "changedModules" (Decode.list moduleChangesDecoder))
        (Decode.oneOf
            [ Decode.field "commentDiffs" (Decode.dict (Decode.dict Decode.string))
            , Decode.succeed Dict.empty
            ]
        )
        (Decode.oneOf
            [ Decode.field "readmeDiff" (Decode.nullable Decode.string)
            , Decode.succeed Nothing
            ]
        )


moduleChangesDecoder : Decoder ModuleChanges
moduleChangesDecoder =
    Decode.map4 ModuleChanges
        (Decode.field "name" Decode.string)
        (Decode.field "added" (Decode.list Decode.string))
        (Decode.field "changed" (Decode.list Decode.string))
        (Decode.field "removed" (Decode.list Decode.string))


lookupItem : ApiDiff -> String -> String -> DiffStatus
lookupItem diff moduleName itemName =
    if List.member moduleName diff.addedModules then
        ModuleAdded

    else
        case findModuleChanges moduleName diff.changedModules of
            Nothing ->
                Unchanged

            Just moduleChanges ->
                if List.member itemName moduleChanges.added then
                    Added

                else if List.member itemName moduleChanges.changed then
                    Changed

                else
                    Unchanged


moduleStatus : ApiDiff -> String -> ModuleStatus
moduleStatus diff moduleName =
    if List.member moduleName diff.addedModules then
        ModuleNew

    else if List.member moduleName diff.removedModules then
        ModuleRemoved

    else if List.any (\mc -> mc.name == moduleName) diff.changedModules then
        ModuleChanged

    else if Dict.member moduleName diff.commentDiffs then
        ModuleChanged

    else
        ModuleUnchanged


findModuleChanges : String -> List ModuleChanges -> Maybe ModuleChanges
findModuleChanges moduleName modules =
    case modules of
        [] ->
            Nothing

        m :: rest ->
            if m.name == moduleName then
                Just m

            else
                findModuleChanges moduleName rest


{-| Get the comment diff for a specific item in a module, if any.
-}
getCommentDiff : ApiDiff -> String -> String -> Maybe String
getCommentDiff diff moduleName itemName =
    Dict.get moduleName diff.commentDiffs
        |> Maybe.andThen (Dict.get itemName)


{-| Get the module-level comment diff, if any.
-}
getModuleCommentDiff : ApiDiff -> String -> Maybe String
getModuleCommentDiff diff moduleName =
    getCommentDiff diff moduleName "__module__"


{-| Check if a module has any comment-only changes (items not in changed/added/removed).
-}
hasCommentChanges : ApiDiff -> String -> Bool
hasCommentChanges diff moduleName =
    Dict.member moduleName diff.commentDiffs


{-| Decode map6 helper since elm/json only goes up to map8.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder value
map6 func da db dc dd de df =
    Decode.map2 (\( a, b, c ) ( d, e, f ) -> func a b c d e f)
        (Decode.map3 (\a b c -> ( a, b, c )) da db dc)
        (Decode.map3 (\d e f -> ( d, e, f )) dd de df)
