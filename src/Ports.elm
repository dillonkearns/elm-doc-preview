port module Ports exposing
    ( Diff, Docs, Manifest, Readme
    , onReadme, onDocs, onManifest, onDiff
    , locationHrefRequested
    )

{-|

@docs Diff, Docs, Manifest, Readme
@docs onReadme, onDocs, onManifest, onDiff
@docs locationHrefRequested

-}

import Json.Encode as Encode


{-| -}
type alias Readme =
    { author : String
    , project : String
    , version : String
    , readme : String
    }


{-| -}
type alias Docs =
    { author : String
    , project : String
    , version : String
    , docs : Encode.Value
    }


{-| -}
type alias Manifest =
    { author : String
    , project : String
    , version : String
    , manifest : Encode.Value
    }


{-| -}
type alias Diff =
    { author : String
    , project : String
    , version : String
    , diff : Encode.Value
    , contentDiff : Encode.Value
    }


{-| -}
port onReadme : (Readme -> msg) -> Sub msg


{-| -}
port onDocs : (Docs -> msg) -> Sub msg


{-| -}
port onManifest : (Manifest -> msg) -> Sub msg


{-| -}
port onDiff : (Diff -> msg) -> Sub msg


{-| -}
port locationHrefRequested : (String -> msg) -> Sub msg
