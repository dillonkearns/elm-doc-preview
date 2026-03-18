module ModuleTree exposing
    ( ModuleTree
    , TreeEntry(..)
    , build
    , entryName
    , filter
    , isLeaf
    , toggle
    , visibleEntries
    )

{-| Module tree that groups Elm modules by shared namespace prefix.

Modules like `Json.Decode` and `Json.Encode` are grouped under a `Json` group
node. Single-module namespaces (like a lone `Http`) remain as flat leaves.

-}

import Dict exposing (Dict)


type ModuleTree
    = ModuleTree (List TreeNode)


type TreeNode
    = GroupNode { prefix : String, expanded : Bool, children : List String }
    | LeafNode String


type TreeEntry
    = Group { prefix : String, expanded : Bool, depth : Int }
    | Leaf { name : String, depth : Int }


{-| Build a module tree from a list of module names.

Groups are created when 2+ modules share a dot-prefix. Groups start expanded.

-}
build : List String -> ModuleTree
build moduleNames =
    let
        -- Group modules by their first segment (before the first dot)
        grouped : Dict String (List String)
        grouped =
            moduleNames
                |> List.foldl
                    (\name acc ->
                        let
                            prefix =
                                String.split "." name
                                    |> List.head
                                    |> Maybe.withDefault name
                        in
                        Dict.update prefix
                            (\existing ->
                                case existing of
                                    Just list ->
                                        Just (list ++ [ name ])

                                    Nothing ->
                                        Just [ name ]
                            )
                            acc
                    )
                    Dict.empty

        -- Convert to tree nodes, preserving original order of first occurrence
        prefixOrder : List String
        prefixOrder =
            moduleNames
                |> List.foldl
                    (\name acc ->
                        let
                            prefix =
                                String.split "." name
                                    |> List.head
                                    |> Maybe.withDefault name
                        in
                        if List.member prefix acc then
                            acc

                        else
                            acc ++ [ prefix ]
                    )
                    []

        nodes : List TreeNode
        nodes =
            prefixOrder
                |> List.filterMap
                    (\prefix ->
                        case Dict.get prefix grouped of
                            Just children ->
                                if List.length children >= 2 then
                                    Just (GroupNode { prefix = prefix, expanded = True, children = children })

                                else
                                    -- Single module in namespace — flat leaf
                                    List.head children
                                        |> Maybe.map LeafNode

                            Nothing ->
                                Nothing
                    )
    in
    ModuleTree nodes


{-| Toggle expand/collapse for a group by prefix name.
-}
toggle : String -> ModuleTree -> ModuleTree
toggle prefix (ModuleTree nodes) =
    ModuleTree
        (List.map
            (\node ->
                case node of
                    GroupNode g ->
                        if g.prefix == prefix then
                            GroupNode { g | expanded = not g.expanded }

                        else
                            node

                    LeafNode _ ->
                        node
            )
            nodes
        )


{-| Filter the tree by a case-insensitive substring match on module names.

Returns a new tree containing only matching modules. Groups are included
if any child matches. Empty filter returns everything.

-}
filter : String -> ModuleTree -> ModuleTree
filter query (ModuleTree nodes) =
    if String.isEmpty query then
        ModuleTree nodes

    else
        let
            lowerQuery =
                String.toLower query
        in
        ModuleTree
            (nodes
                |> List.filterMap
                    (\node ->
                        case node of
                            GroupNode g ->
                                let
                                    matchingChildren =
                                        g.children
                                            |> List.filter
                                                (\name ->
                                                    String.contains lowerQuery (String.toLower name)
                                                )
                                in
                                if List.isEmpty matchingChildren then
                                    Nothing

                                else
                                    Just (GroupNode { g | children = matchingChildren })

                            LeafNode name ->
                                if String.contains lowerQuery (String.toLower name) then
                                    Just node

                                else
                                    Nothing
                    )
            )


{-| Get the flat list of visible entries, respecting collapsed groups.
-}
visibleEntries : ModuleTree -> List TreeEntry
visibleEntries (ModuleTree nodes) =
    nodes
        |> List.concatMap
            (\node ->
                case node of
                    GroupNode g ->
                        let
                            groupEntry =
                                Group { prefix = g.prefix, expanded = g.expanded, depth = 0 }
                        in
                        if g.expanded then
                            groupEntry
                                :: List.map
                                    (\name -> Leaf { name = name, depth = 1 })
                                    g.children

                        else
                            [ groupEntry ]

                    LeafNode name ->
                        [ Leaf { name = name, depth = 0 } ]
            )


{-| Get the display name for a tree entry.
-}
entryName : TreeEntry -> String
entryName entry =
    case entry of
        Leaf { name } ->
            name

        Group { prefix } ->
            prefix


{-| Check if a tree entry is a leaf (module) vs a group header.
-}
isLeaf : TreeEntry -> Bool
isLeaf entry =
    case entry of
        Leaf _ ->
            True

        Group _ ->
            False
