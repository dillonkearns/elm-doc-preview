module Docs.Block exposing (toBlocks)

import Elm.Docs as Docs


{-| Parse a module's comment and declarations into an ordered list of blocks.
Delegates to Elm.Docs.toBlocks which handles @docs directive parsing.
-}
toBlocks : Docs.Module -> List Docs.Block
toBlocks =
    Docs.toBlocks
