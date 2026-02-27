module Session exposing
    ( Data, Docs(..), Preview, RepoDocsResponse, empty
    , addDocs, addEntries, addManifest, addReadme, addReleases, addPreview, addDiff
    , fetchDocs, fetchManifest, fetchReadme, fetchReleases, fetchPreview, fetchRepoDocs
    , getDocs, getEntries, getManifest, getReadme, getReleases, getPreview, getDiff, getRepoDocs
    , addRepoDocs
    , docsDecoder
    )

{-|

@docs Data, Docs, Preview, RepoDocsResponse, empty
@docs addDocs, addEntries, addManifest, addReadme, addReleases, addPreview, addDiff
@docs fetchDocs, fetchManifest, fetchReadme, fetchReleases, fetchPreview, fetchRepoDocs
@docs getDocs, getEntries, getManifest, getReadme, getReleases, getPreview, getDiff, getRepoDocs
@docs addRepoDocs
@docs docsDecoder

-}

import ApiDiff exposing (ApiDiff)
import Dict exposing (Dict)
import Elm.Docs as Docs
import Elm.Error exposing (Error)
import Elm.Project as Project exposing (Project)
import Elm.Version as V
import Http
import Json.Decode as Decode exposing (Decoder)
import Page.Search.Entry as Entry
import Parser
import Release exposing (Release)
import Url.Builder as Url
import Utils.OneOrMore exposing (OneOrMore(..))



-- SESSION DATA


{-| -}
type alias Data =
    { entries : Maybe (Dict String Entry.Entry)
    , releases : Dict.Dict String (OneOrMore Release.Release)
    , readmes : Dict.Dict String String
    , docs : Dict.Dict String Docs
    , manifests : Dict.Dict String Project
    , preview : Maybe Preview
    , diff : Maybe ApiDiff
    }


{-| -}
type Docs
    = Error Error
    | Modules (List Docs.Module)


{-| -}
type alias Preview =
    { name : String
    , version : String
    }


{-| -}
type alias RepoDocsResponse =
    { docs : Docs
    , diff : Maybe ApiDiff
    }


{-| -}
empty : Data
empty =
    { entries = Nothing
    , releases = Dict.empty
    , readmes = Dict.empty
    , docs = Dict.empty
    , manifests = Dict.empty
    , preview = Nothing
    , diff = Nothing
    }



-- PREVIEW


{-| -}
getPreview : Data -> Maybe Preview
getPreview data =
    data.preview


{-| -}
addPreview : Preview -> Data -> Data
addPreview preview data =
    { data | preview = Just preview }


{-| -}
fetchPreview : (Result Http.Error Preview -> msg) -> Cmd msg
fetchPreview toMsg =
    Http.get
        { url = Url.absolute [ "preview" ] []
        , expect = Http.expectJson toMsg previewDecoder
        }


previewDecoder : Decoder Preview
previewDecoder =
    Decode.map2 Preview
        (Decode.field "name" Decode.string)
        (Decode.field "version" Decode.string)



-- ENTRIES


{-| -}
getEntries : Data -> Maybe (Dict String Entry.Entry)
getEntries data =
    data.entries


{-| -}
addEntries : Dict String Entry.Entry -> Data -> Data
addEntries entries data =
    { data | entries = Just entries }



-- RELEASES


{-| -}
toPkgKey : String -> String -> String
toPkgKey author project =
    author ++ "/" ++ project


{-| -}
getReleases : Data -> String -> String -> Maybe (OneOrMore Release)
getReleases data author project =
    Dict.get (toPkgKey author project) data.releases


{-| -}
addReleases : String -> String -> OneOrMore Release -> Data -> Data
addReleases author project releases data =
    let
        newReleases =
            Dict.insert (toPkgKey author project) releases data.releases
    in
    { data | releases = newReleases }


{-| -}
fetchReleases : (Result Http.Error (OneOrMore Release) -> msg) -> String -> String -> Cmd msg
fetchReleases toMsg author project =
    Http.get
        { url = Url.absolute [ "packages", author, project, "releases.json" ] []
        , expect = Http.expectJson toMsg Release.decoder
        }



-- README


{-| -}
toVsnKey : String -> String -> V.Version -> String
toVsnKey author project version =
    author ++ "/" ++ project ++ "@" ++ V.toString version


{-| -}
getReadme : Data -> String -> String -> V.Version -> Maybe String
getReadme data author project version =
    Dict.get (toVsnKey author project version) data.readmes


{-| -}
addReadme : String -> String -> V.Version -> String -> Data -> Data
addReadme author project version readme data =
    let
        newReadmes =
            Dict.insert (toVsnKey author project version) readme data.readmes
    in
    { data | readmes = newReadmes }


{-| -}
fetchReadme : (Result Http.Error String -> msg) -> String -> String -> V.Version -> Cmd msg
fetchReadme toMsg author project version =
    Http.get
        { url = Url.absolute [ "packages", author, project, V.toString version, "README.md" ] []
        , expect = Http.expectString toMsg
        }



-- DOCS


{-| -}
getDocs : Data -> String -> String -> V.Version -> Maybe Docs
getDocs data author project version =
    Dict.get (toVsnKey author project version) data.docs


{-| -}
addDocs : String -> String -> V.Version -> Docs -> Data -> Data
addDocs author project version docs data =
    let
        newDocs =
            Dict.insert (toVsnKey author project version) docs data.docs
    in
    { data | docs = newDocs }


{-| -}
fetchDocs : (Result Http.Error Docs -> msg) -> String -> String -> V.Version -> Cmd msg
fetchDocs toMsg author project version =
    Http.get
        { url = Url.absolute [ "packages", author, project, V.toString version, "docs.json" ] []
        , expect = Http.expectJson toMsg docsDecoder
        }


{-| -}
docsDecoder : Decoder Docs
docsDecoder =
    Decode.oneOf
        [ Decode.map Modules (Decode.list Docs.decoder)
        , Decode.map Error Elm.Error.decoder
        ]



-- MANIFEST


{-| -}
getManifest : Data -> String -> String -> V.Version -> Maybe Project
getManifest data author project version =
    Dict.get (toVsnKey author project version) data.manifests


{-| -}
addManifest : String -> String -> V.Version -> Project -> Data -> Data
addManifest author project version manifest data =
    let
        newManifests =
            Dict.insert (toVsnKey author project version) manifest data.manifests
    in
    { data | manifests = newManifests }


{-| -}
fetchManifest :
    (Result Http.Error Project -> msg)
    -> String
    -> String
    -> V.Version
    -> Cmd msg
fetchManifest toMsg author project version =
    Http.get
        { url = Url.absolute [ "packages", author, project, V.toString version, "elm.json" ] []
        , expect = Http.expectJson toMsg Project.decoder
        }



-- REPO DOCS


{-| -}
toRefKey : String -> String -> String -> String
toRefKey owner repo ref =
    "repo:" ++ owner ++ "/" ++ repo ++ "@" ++ ref


{-| -}
getRepoDocs : Data -> String -> String -> String -> Maybe Docs
getRepoDocs data owner repo ref =
    Dict.get (toRefKey owner repo ref) data.docs


{-| -}
addRepoDocs : String -> String -> String -> RepoDocsResponse -> Data -> Data
addRepoDocs owner repo ref response data =
    let
        newDocs =
            Dict.insert (toRefKey owner repo ref) response.docs data.docs
    in
    { data | docs = newDocs, diff = response.diff }


{-| -}
fetchRepoDocs : (Result Http.Error RepoDocsResponse -> msg) -> String -> String -> String -> Cmd msg
fetchRepoDocs toMsg owner repo ref =
    Http.get
        { url = Url.absolute [ "repos", owner, repo, ref, "preview.json" ] []
        , expect = Http.expectJson toMsg repoDocsResponseDecoder
        }


repoDocsResponseDecoder : Decoder RepoDocsResponse
repoDocsResponseDecoder =
    Decode.map2 RepoDocsResponse
        (Decode.field "docs" docsDecoder)
        (Decode.field "diff" ApiDiff.decoder)



-- DIFF


{-| -}
getDiff : Data -> Maybe ApiDiff
getDiff data =
    data.diff


{-| -}
addDiff : Maybe ApiDiff -> Data -> Data
addDiff maybeDiff data =
    { data | diff = maybeDiff }
