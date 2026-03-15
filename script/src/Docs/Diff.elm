module Docs.Diff exposing
    ( ApiDiff
    , DiffStatus(..)
    , ModuleChanges
    , ModuleStatus(..)
    , decoder
    , findModuleChanges
    , lookupItem
    , moduleStatus
    )

import Json.Decode as Decode exposing (Decoder)


type alias ApiDiff =
    { magnitude : String
    , addedModules : List String
    , removedModules : List String
    , changedModules : List ModuleChanges
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
    Decode.map4 ApiDiff
        (Decode.field "magnitude" Decode.string)
        (Decode.field "addedModules" (Decode.list Decode.string))
        (Decode.field "removedModules" (Decode.list Decode.string))
        (Decode.field "changedModules" (Decode.list moduleChangesDecoder))


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
