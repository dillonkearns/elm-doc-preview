module Page.Docs exposing
    ( Focus(..), Model, Msg
    , init, initRepo, update, view
    , toTitle
    , updateReadme, updateDocs, updateManifest, updateDiff, updateContentDiff
    )

{-|

@docs Focus, Model, Msg
@docs init, initRepo, update, view
@docs toTitle
@docs updateReadme, updateDocs, updateManifest, updateDiff, updateContentDiff

-}

import ApiDiff exposing (ApiDiff, DiffStatus(..))
import Browser.Dom as Dom
import ContentDiff exposing (ContentDiff)
import DateFormat
import Elm.Constraint as Constraint exposing (Constraint)
import Elm.Docs as Docs
import Elm.License as License
import Elm.Package as Package
import Elm.Project as Project exposing (Project)
import Elm.Version as Version exposing (Version)
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Page.Docs.Block as Block
import Page.Problem as Problem
import Release
import Session exposing (Docs(..))
import Skeleton
import Task
import Time
import Url.Builder as Url
import Utils.Error
import Utils.Markdown as Markdown
import Utils.OneOrMore exposing (OneOrMore)
import Utils.Spinner



-- MODEL


{-| -}
type alias Model =
    { session : Session.Data
    , author : String
    , project : String
    , version : Maybe Version
    , ref : Maybe String
    , focus : Focus
    , query : String
    , latest : Status Version
    , readme : Status String
    , docs : Status Docs
    , manifest : Status Project
    , diffData : Maybe ApiDiff
    , diffMode : Bool
    , pullRequestUrl : Maybe String
    , contentDiff : Maybe ContentDiff
    }


{-| -}
type Focus
    = Readme (Maybe String)
    | Module String (Maybe String)


type Status a
    = Failure
    | Loading
    | Success a



-- INIT


{-| -}
init : Session.Data -> String -> String -> Maybe Version -> Focus -> ( Model, Cmd Msg )
init session author project version focus =
    let
        restoredDiff =
            Session.getDiff session
    in
    case Session.getReleases session author project of
        Just releases ->
            let
                latest =
                    Release.getLatest releases
            in
            getInfo latest <|
                Model session author project version Nothing focus "" (Success latest) Loading Loading Loading restoredDiff (restoredDiff /= Nothing) Nothing Nothing

        Nothing ->
            ( Model session author project version Nothing focus "" Loading Loading Loading Loading restoredDiff (restoredDiff /= Nothing) Nothing Nothing
            , Session.fetchReleases GotReleases author project
            )


{-| -}
initRepo : Session.Data -> String -> String -> String -> Focus -> ( Model, Cmd Msg )
initRepo session owner repo ref focus =
    let
        restoredDiff =
            Session.getDiff session
    in
    case Session.getRepoDocs session owner repo ref of
        Just docs ->
            ( Model session owner repo Nothing (Just ref) focus "" Loading Loading (Success docs) Loading restoredDiff (restoredDiff /= Nothing) Nothing Nothing
            , scrollIfNeeded focus
            )

        Nothing ->
            ( Model session owner repo Nothing (Just ref) focus "" Loading Loading Loading Loading Nothing False Nothing Nothing
            , Session.fetchRepoDocs GotRepoDocs owner repo ref
            )


getInfo : Version -> Model -> ( Model, Cmd Msg )
getInfo latest model =
    let
        author =
            model.author

        project =
            model.project

        version =
            Maybe.withDefault latest model.version

        maybeInfo =
            Maybe.map3 (\readme docs manifest -> ( readme, docs, manifest ))
                (Session.getReadme model.session author project version)
                (Session.getDocs model.session author project version)
                (Session.getManifest model.session author project version)
    in
    case maybeInfo of
        Nothing ->
            ( model
            , Cmd.batch
                [ Session.fetchReadme (GotReadme version) author project version
                , Session.fetchDocs (GotDocs version) author project version
                , Session.fetchManifest (GotManifest version) author project version
                ]
            )

        Just ( readme, docs, manifest ) ->
            ( { model
                | readme = Success readme
                , docs = Success docs
                , manifest = Success manifest
              }
            , scrollIfNeeded model.focus
            )


scrollIfNeeded : Focus -> Cmd Msg
scrollIfNeeded focus =
    let
        scrollToTag tag =
            Dom.getElement tag
                |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
                |> Task.attempt ScrollAttempted
    in
    case focus of
        Readme (Just tag) ->
            scrollToTag tag

        Module _ (Just tag) ->
            scrollToTag tag

        _ ->
            Cmd.none



-- UPDATE


{-| -}
type Msg
    = QueryChanged String
    | ScrollAttempted (Result Dom.Error ())
    | ToggleDiffMode
    | GotReleases (Result Http.Error (OneOrMore Release.Release))
    | GotReadme Version (Result Http.Error String)
    | GotDocs Version (Result Http.Error Docs)
    | GotManifest Version (Result Http.Error Project)
    | GotRepoDocs (Result Http.Error Session.RepoDocsResponse)


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged query ->
            ( { model | query = query }
            , Cmd.none
            )

        ScrollAttempted _ ->
            ( model
            , Cmd.none
            )

        ToggleDiffMode ->
            ( { model | diffMode = not model.diffMode }
            , Cmd.none
            )

        GotReleases result ->
            case result of
                Err _ ->
                    ( { model
                        | latest = Failure
                        , readme = Failure
                        , docs = Failure
                      }
                    , Cmd.none
                    )

                Ok releases ->
                    let
                        latest =
                            Release.getLatest releases
                    in
                    getInfo latest
                        { model
                            | latest = Success latest
                            , session = Session.addReleases model.author model.project releases model.session
                            , version =
                                if model.version == Nothing then
                                    Just latest

                                else
                                    model.version
                        }

        GotReadme version result ->
            case result of
                Err _ ->
                    ( { model | readme = Failure }
                    , Cmd.none
                    )

                Ok readme ->
                    ( { model
                        | readme = Success readme
                        , session = Session.addReadme model.author model.project version readme model.session
                      }
                    , Cmd.none
                    )

        GotDocs version result ->
            case result of
                Err _ ->
                    ( { model | docs = Failure }
                    , Cmd.none
                    )

                Ok docs ->
                    ( { model
                        | docs = Success docs
                        , session = Session.addDocs model.author model.project version docs model.session
                      }
                    , scrollIfNeeded model.focus
                    )

        GotManifest version result ->
            case result of
                Err err ->
                    ( { model | manifest = Failure }
                    , Cmd.none
                    )

                Ok manifest ->
                    ( { model
                        | manifest = Success manifest
                        , session = Session.addManifest model.author model.project version manifest model.session
                      }
                    , Cmd.none
                    )

        GotRepoDocs result ->
            case ( result, model.ref ) of
                ( Ok response, Just ref ) ->
                    ( { model
                        | docs = Success response.docs
                        , diffData = response.diff
                        , diffMode = response.diff /= Nothing
                        , pullRequestUrl = response.pullRequestUrl
                        , contentDiff = response.contentDiff
                        , session = Session.addRepoDocs model.author model.project ref response model.session
                      }
                    , scrollIfNeeded model.focus
                    )

                ( Err _, _ ) ->
                    ( { model | docs = Failure }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- EXTERNAL UPDATES


{-| -}
updateReadme : String -> String -> Version -> String -> Model -> Model
updateReadme author project version readme model =
    let
        newSession =
            Session.addReadme author project version readme model.session
    in
    if author == model.author && project == model.project && Just version == model.version then
        { model | session = newSession, readme = Success readme }

    else
        { model | session = newSession }


{-| -}
updateDocs : String -> String -> Version -> Docs -> Model -> Model
updateDocs author project version docs model =
    let
        newSession =
            Session.addDocs author project version docs model.session
    in
    if author == model.author && project == model.project && Just version == model.version then
        { model | session = newSession, docs = Success docs }

    else
        { model | session = newSession }


{-| -}
updateManifest : String -> String -> Version -> Project -> Model -> Model
updateManifest author project version manifest model =
    let
        newSession =
            Session.addManifest author project version manifest model.session
    in
    if author == model.author && project == model.project && Just version == model.version then
        { model | session = newSession, manifest = Success manifest }

    else
        { model | session = newSession }


{-| -}
updateDiff : String -> String -> Version -> Maybe ApiDiff -> Model -> Model
updateDiff author project version maybeDiff model =
    if author == model.author && project == model.project && Just version == model.version then
        { model | diffData = maybeDiff }

    else
        model


{-| -}
updateContentDiff : Maybe ContentDiff -> Model -> Model
updateContentDiff maybeContentDiff model =
    { model | contentDiff = maybeContentDiff }



-- VIEW


{-| -}
view : Model -> Skeleton.Details Msg
view model =
    { title = toTitle model
    , header = toHeader model
    , warning = toWarning model
    , attrs = []
    , kids =
        [ viewContent model
        , viewSidebar model
        ]
    }



-- TITLE


{-| -}
toTitle : Model -> String
toTitle model =
    case model.focus of
        Readme _ ->
            toGenericTitle model

        Module name _ ->
            name ++ " - " ++ toGenericTitle model


toGenericTitle : Model -> String
toGenericTitle model =
    case model.ref of
        Just ref ->
            model.author ++ "/" ++ model.project ++ " (" ++ ref ++ ")"

        Nothing ->
            case getVersion model of
                Just version ->
                    model.project ++ " " ++ Version.toString version

                Nothing ->
                    model.project


getVersion : Model -> Maybe Version
getVersion model =
    case model.version of
        Just version ->
            model.version

        Nothing ->
            case model.latest of
                Success version ->
                    Just version

                Loading ->
                    Nothing

                Failure ->
                    Nothing



-- TO HEADER


toHeader : Model -> List Skeleton.Segment
toHeader model =
    case model.ref of
        Just ref ->
            [ Skeleton.Text model.author
            , Skeleton.Link (Href.toRepoVersion model.author model.project ref Nothing) model.project
            , Skeleton.Text ref
            ]

        Nothing ->
            [ Skeleton.authorSegment model.author
            , Skeleton.projectSegment model.author model.project
            , Skeleton.versionSegment model.author model.project (getVersion model)
            ]



-- WARNING


toWarning : Model -> Skeleton.Warning
toWarning model =
    case model.ref of
        Just _ ->
            Skeleton.NoProblems

        Nothing ->
            case model.version of
                Nothing ->
                    Skeleton.NoProblems

                Just version ->
                    case model.latest of
                        Success latest ->
                            if version == latest then
                                Skeleton.NoProblems

                            else
                                Skeleton.NewerVersion (toNewerUrl model) latest

                        Loading ->
                            Skeleton.NoProblems

                        Failure ->
                            Skeleton.NoProblems


toNewerUrl : Model -> String
toNewerUrl model =
    case model.focus of
        Readme tag ->
            Href.toVersion model.author model.project Nothing tag

        Module name tag ->
            Href.toModule model.author model.project Nothing name tag



-- VIEW CONTENT


viewContent : Model -> Html msg
viewContent model =
    case model.focus of
        Readme _ ->
            case model.docs of
                Success (Error error) ->
                    lazy Utils.Error.view error

                _ ->
                    viewReadme model.readme model.diffMode model.contentDiff

        Module name tag ->
            viewModule model.author model.project model.version name model.docs model.diffData model.diffMode model.contentDiff



-- VIEW README


viewReadme : Status String -> Bool -> Maybe ContentDiff -> Html msg
viewReadme status diffMode maybeContentDiff =
    case status of
        Success readme ->
            let
                readmeDiffView =
                    if diffMode then
                        case maybeContentDiff |> Maybe.andThen ContentDiff.getReadmeDiff of
                            Just diffLines ->
                                [ viewDiffBlock diffLines ]

                            Nothing ->
                                []

                    else
                        []
            in
            div [ class "block-list" ] (readmeDiffView ++ [ Markdown.block readme ])

        Loading ->
            Utils.Spinner.view

        -- TODO
        Failure ->
            div
                (class "block-list" :: Problem.styles)
                (Problem.offline "README.md")



viewDiffBlock : List ContentDiff.DiffLine -> Html msg
viewDiffBlock lines =
    pre [ class "diff-view" ]
        (List.map viewDiffLine lines)


viewDiffLine : ContentDiff.DiffLine -> Html msg
viewDiffLine { status, content } =
    let
        ( lineClass, prefix ) =
            case status of
                ContentDiff.DiffAdded ->
                    ( "diff-line diff-line-added", "+ " )

                ContentDiff.DiffRemoved ->
                    ( "diff-line diff-line-removed", "- " )

                ContentDiff.DiffContext ->
                    ( "diff-line diff-line-context", "  " )
    in
    div [ class lineClass ]
        [ span [ class "diff-line-prefix" ] [ text prefix ]
        , text content
        ]



-- VIEW MODULE


viewModule : String -> String -> Maybe Version -> String -> Status Docs -> Maybe ApiDiff -> Bool -> Maybe ContentDiff -> Html msg
viewModule author project version name status diffData diffMode contentDiff =
    case status of
        Success (Modules allDocs) ->
            case findModule name allDocs of
                Just docs ->
                    let
                        header =
                            h1 [ class "block-list-title" ] [ text name ]

                        info =
                            Block.makeInfo author project version name allDocs diffData diffMode contentDiff

                        blocks =
                            List.map (Block.view info) (Docs.toBlocks docs)
                    in
                    div [ class "block-list" ] (header :: blocks)

                Nothing ->
                    div
                        (class "block-list" :: Problem.styles)
                        (Problem.missingModule author project version name)

        Success (Error error) ->
            lazy Utils.Error.view error

        Loading ->
            div [ class "block-list" ]
                [ h1 [ class "block-list-title" ] [ text name ] -- TODO better loading
                , Utils.Spinner.view
                ]

        Failure ->
            div
                (class "block-list" :: Problem.styles)
                (Problem.offline "docs.json")


findModule : String -> List Docs.Module -> Maybe Docs.Module
findModule name docsList =
    case docsList of
        [] ->
            Nothing

        docs :: otherDocs ->
            if docs.name == name then
                Just docs

            else
                findModule name otherDocs



-- VIEW SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
    div
        [ class "pkg-nav"
        ]
        (viewDiffToggle model.diffData
            :: (if model.diffMode then
                    case model.diffData of
                        Just diff ->
                            [ viewDiffSidebar model diff ]

                        Nothing ->
                            viewNormalSidebar model

                else
                    viewNormalSidebar model
               )
        )


viewNormalSidebar : Model -> List (Html Msg)
viewNormalSidebar model =
    [ viewReadmeLink model.author model.project model.version model.ref model.focus
    , br [] []
    , case model.ref of
        Just ref ->
            a [ class "pkg-nav-module", href ("https://github.com/" ++ model.author ++ "/" ++ model.project ++ "/tree/" ++ ref) ]
                [ text "Source" ]

        Nothing ->
            lazy4 viewBrowseSourceLink model.author model.project model.version model.latest
    , h2 [ style "margin-bottom" "0" ] [ text "Modules" ]
    , input
        [ placeholder "Search"
        , value model.query
        , onInput QueryChanged
        ]
        []
    , viewSidebarModules model
    , viewInstall model.manifest model.author model.project
    , viewLicense model.manifest
    , viewDependencies model.manifest
    ]


viewDiffToggle : Maybe ApiDiff -> Html Msg
viewDiffToggle maybeDiff =
    case maybeDiff of
        Just diff ->
            if ApiDiff.hasChanges diff then
                button
                    [ class "diff-toggle"
                    , onClick ToggleDiffMode
                    ]
                    [ text "Toggle API Diff" ]

            else
                text ""

        Nothing ->
            text ""


viewDiffSidebar : Model -> ApiDiff -> Html Msg
viewDiffSidebar model diff =
    let
        magnitudeClass =
            case diff.magnitude of
                "MAJOR" ->
                    "diff-magnitude diff-magnitude-major"

                "MINOR" ->
                    "diff-magnitude diff-magnitude-minor"

                _ ->
                    "diff-magnitude diff-magnitude-patch"

        query =
            model.query |> String.toLower |> String.trim
    in
    div [ class "diff-sidebar" ]
        [ viewPullRequestLink model.pullRequestUrl
        , h2 []
            [ text "API Diff "
            , span [ class magnitudeClass ] [ text diff.magnitude ]
            ]
        , if not (ApiDiff.hasChanges diff) then
            p [] [ text "No API changes" ]

          else
            div []
                [ input
                    [ placeholder "Search"
                    , value model.query
                    , onInput QueryChanged
                    ]
                    []
                , div []
                    (viewFilteredDiffAddedModules model query diff.addedModules
                        ++ viewFilteredDiffRemovedModules query diff.removedModules
                        ++ List.concatMap (viewFilteredDiffChangedModule model query) diff.changedModules
                    )
                ]
        ]


viewPullRequestLink : Maybe String -> Html msg
viewPullRequestLink maybePrUrl =
    case maybePrUrl of
        Just prUrl ->
            a
                [ href prUrl
                , class "pkg-nav-module"
                , target "_blank"
                ]
                [ text "View Pull Request" ]

        Nothing ->
            text ""


viewFilteredDiffAddedModules : Model -> String -> List String -> List (Html Msg)
viewFilteredDiffAddedModules model query modules =
    let
        filtered =
            if String.isEmpty query then
                modules

            else
                List.filter (\name -> String.contains query (String.toLower name)) modules
    in
    viewDiffAddedModules model filtered


viewFilteredDiffRemovedModules : String -> List String -> List (Html msg)
viewFilteredDiffRemovedModules query modules =
    let
        filtered =
            if String.isEmpty query then
                modules

            else
                List.filter (\name -> String.contains query (String.toLower name)) modules
    in
    viewDiffRemovedModules filtered


viewFilteredDiffChangedModule : Model -> String -> ApiDiff.ModuleChanges -> List (Html Msg)
viewFilteredDiffChangedModule model query mc =
    if String.isEmpty query then
        viewDiffChangedModule model mc

    else
        let
            nameMatches =
                String.contains query (String.toLower mc.name)

            filteredMc =
                if nameMatches then
                    mc

                else
                    filterChangedModule query mc
        in
        if nameMatches || not (List.isEmpty filteredMc.added && List.isEmpty filteredMc.changed && List.isEmpty filteredMc.removed) then
            viewDiffChangedModule model filteredMc

        else
            []


filterChangedModule : String -> ApiDiff.ModuleChanges -> ApiDiff.ModuleChanges
filterChangedModule query mc =
    let
        matchItem name =
            String.contains query (String.toLower name)
    in
    { mc
        | added = List.filter matchItem mc.added
        , changed = List.filter matchItem mc.changed
        , removed = List.filter matchItem mc.removed
    }


viewDiffAddedModules : Model -> List String -> List (Html Msg)
viewDiffAddedModules model modules =
    if List.isEmpty modules then
        []

    else
        [ h3 [ class "diff-section-title diff-item-added" ] [ text "Added Modules" ]
        , ul []
            (List.map
                (\name ->
                    li [ class "diff-item-added" ]
                        [ span [ class "diff-prefix" ] [ text "+" ]
                        , viewModuleLink model name
                        ]
                )
                modules
            )
        ]


viewDiffRemovedModules : List String -> List (Html msg)
viewDiffRemovedModules modules =
    if List.isEmpty modules then
        []

    else
        [ h3 [ class "diff-section-title diff-item-removed" ] [ text "Removed Modules" ]
        , ul []
            (List.map
                (\name ->
                    li [ class "diff-item-removed" ]
                        [ span [ class "diff-prefix" ] [ text "-" ]
                        , span [ class "diff-removed-module" ] [ text name ]
                        ]
                )
                modules
            )
        ]


viewDiffChangedModule : Model -> ApiDiff.ModuleChanges -> List (Html Msg)
viewDiffChangedModule model mc =
    [ h3 [ class "diff-section-title" ] [ viewModuleLink model mc.name ]
    ]
        ++ viewDiffItemList model mc.name "diff-item-added" "+" mc.added
        ++ viewDiffItemList model mc.name "diff-item-changed" "~" mc.changed
        ++ viewDiffRemovedItems mc.removed


viewDiffItemList : Model -> String -> String -> String -> List String -> List (Html Msg)
viewDiffItemList model moduleName cssClass prefix items =
    if List.isEmpty items then
        []

    else
        let
            toModuleHref itemName =
                case model.ref of
                    Just ref ->
                        Href.toRepoModule model.author model.project ref moduleName (Just itemName)

                    Nothing ->
                        Href.toModule model.author model.project model.version moduleName (Just itemName)
        in
        [ ul []
            (List.map
                (\name ->
                    li [ class cssClass ]
                        [ span [ class "diff-prefix" ] [ text prefix ]
                        , a
                            [ href (toModuleHref name)
                            , class "pkg-nav-module"
                            ]
                            [ text name ]
                        ]
                )
                items
            )
        ]


viewDiffRemovedItems : List String -> List (Html msg)
viewDiffRemovedItems items =
    if List.isEmpty items then
        []

    else
        [ ul []
            (List.map
                (\name ->
                    li [ class "diff-item-removed" ]
                        [ span [ class "diff-prefix" ] [ text "-" ]
                        , text name
                        ]
                )
                items
            )
        ]


viewSidebarModules : Model -> Html msg
viewSidebarModules model =
    case model.docs of
        Failure ->
            text ""

        -- TODO
        Loading ->
            text ""

        Success (Modules modules) ->
            if String.isEmpty model.query then
                let
                    viewEntry docs =
                        li [] [ viewModuleLink model docs.name ]
                in
                ul [] (List.map viewEntry modules)

            else
                let
                    query =
                        model.query |> String.toLower |> String.trim
                in
                ul [] (List.filterMap (viewSearchItem model query) modules)

        Success (Error _) ->
            text ""


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html msg)
viewSearchItem model query docs =
    let
        toItem ownerName valueName =
            viewValueItem model docs.name ownerName valueName

        matches =
            List.filterMap (isMatch query toItem) docs.binops
                ++ List.concatMap (isUnionMatch query toItem) docs.unions
                ++ List.filterMap (isMatch query toItem) docs.aliases
                ++ List.filterMap (isMatch query toItem) docs.values
    in
    if List.isEmpty matches && not (String.contains query docs.name) then
        Nothing

    else
        Just <|
            li
                [ class "pkg-nav-search-chunk"
                ]
                [ viewModuleLink model docs.name
                , ul [] matches
                ]


isMatch : String -> (String -> String -> b) -> { r | name : String } -> Maybe b
isMatch query toResult { name } =
    if String.contains query (String.toLower name) then
        Just (toResult name name)

    else
        Nothing


isUnionMatch : String -> (String -> String -> a) -> Docs.Union -> List a
isUnionMatch query toResult { name, tags } =
    let
        tagMatches =
            List.filterMap (isTagMatch query toResult name) tags
    in
    if String.contains query (String.toLower name) then
        toResult name name :: tagMatches

    else
        tagMatches


isTagMatch : String -> (String -> String -> a) -> String -> ( String, details ) -> Maybe a
isTagMatch query toResult tipeName ( tagName, _ ) =
    if String.contains query (String.toLower tagName) then
        Just (toResult tipeName tagName)

    else
        Nothing



-- VIEW "README" LINK


viewReadmeLink : String -> String -> Maybe Version -> Maybe String -> Focus -> Html msg
viewReadmeLink author project version ref focus =
    let
        url =
            case ref of
                Just r ->
                    Href.toRepoVersion author project r Nothing

                Nothing ->
                    Href.toVersion author project version Nothing
    in
    navLink "README" url <|
        case focus of
            Readme _ ->
                True

            Module _ _ ->
                False



-- VIEW "SOURCE" LINK


viewBrowseSourceLink : String -> String -> Maybe Version -> Status Version -> Html msg
viewBrowseSourceLink author project maybeVersion latest =
    case maybeVersion of
        Just version ->
            viewBrowseSourceLinkHelp author project version

        Nothing ->
            case latest of
                Success version ->
                    viewBrowseSourceLinkHelp author project version

                Loading ->
                    text "Source"

                Failure ->
                    text "Source"


viewBrowseSourceLinkHelp : String -> String -> Version -> Html msg
viewBrowseSourceLinkHelp author project version =
    let
        url =
            Url.absolute
                [ "source", author, project, Version.toString version ]
                []
    in
    a [ class "pkg-nav-module", href url ] [ text "Source" ]



-- VIEW "MODULE" LINK


viewModuleLink : Model -> String -> Html msg
viewModuleLink model name =
    let
        url =
            case model.ref of
                Just ref ->
                    Href.toRepoModule model.author model.project ref name Nothing

                Nothing ->
                    Href.toModule model.author model.project model.version name Nothing
    in
    navLink name url <|
        case model.focus of
            Readme _ ->
                False

            Module selectedName _ ->
                selectedName == name


viewValueItem : Model -> String -> String -> String -> Html msg
viewValueItem model moduleName ownerName valueName =
    let
        url =
            case model.ref of
                Just ref ->
                    Href.toRepoModule model.author model.project ref moduleName (Just ownerName)

                Nothing ->
                    Href.toModule model.author model.project model.version moduleName (Just ownerName)
    in
    li [ class "pkg-nav-value" ] [ navLink valueName url False ]



-- LINK HELPERS


navLink : String -> String -> Bool -> Html msg
navLink name url isBold =
    let
        attributes =
            if isBold then
                [ class "pkg-nav-module"
                , style "font-weight" "bold"
                , style "text-decoration" "underline"
                ]

            else
                [ class "pkg-nav-module"
                ]
    in
    a (href url :: attributes) [ text name ]



-- VIEW INSTALL


viewInstall : Status Project -> String -> String -> Html msg
viewInstall manifest author project =
    case manifest of
        Success (Project.Package info) ->
            let
                install =
                    "elm install " ++ author ++ "/" ++ project
            in
            div []
                [ h2 [] [ text "Install" ]
                , pre
                    [ class "copy-to-clipboard"
                    , attribute "data-clipboard-text" install
                    ]
                    [ text install
                    ]
                ]

        _ ->
            text ""



-- VIEW LICENSE


viewLicense : Status Project -> Html msg
viewLicense manifest =
    case manifest of
        Success (Project.Package info) ->
            let
                licenseUrl =
                    Url.absolute
                        [ "source"
                        , Package.toString info.name
                        , Version.toString info.version
                        , "LICENSE"
                        ]
                        []
            in
            div []
                [ h2 [] [ text "License" ]
                , div []
                    [ a [ href licenseUrl ]
                        [ nowrap []
                            [ License.toString info.license ]
                        ]
                    ]
                ]

        _ ->
            text ""


nowrap : List (Attribute msg) -> List String -> Html msg
nowrap attrs children =
    span
        (style "white-space" "nowrap" :: attrs)
        (List.map text children)



-- VIEW DEPENDENCIES


viewDependencies : Status Project -> Html msg
viewDependencies manifest =
    case manifest of
        Success (Project.Package info) ->
            div []
                (h2 [] [ text "Dependencies" ]
                    :: viewElmVersion Constraint.toString info.elm
                    :: List.map viewDepConstraint info.deps
                )

        Success (Project.Application info) ->
            div []
                (h2 [] [ text "Dependencies" ]
                    :: viewElmVersion Version.toString info.elm
                    :: List.map viewDepVersion info.depsDirect
                )

        _ ->
            text ""


viewElmVersion : (version -> String) -> version -> Html msg
viewElmVersion versionToString version =
    div [ style "white-space" "nowrap" ]
        [ text ("elm " ++ versionToString version)
        ]


viewDepConstraint : ( Package.Name, Constraint ) -> Html msg
viewDepConstraint ( name, constraint ) =
    div [ style "white-space" "nowrap" ]
        [ a
            [ href <|
                Url.absolute [ "packages", Package.toString name, "latest" ] []
            ]
            [ text (Package.toString name) ]
        , text (" " ++ Constraint.toString constraint)
        ]


viewDepVersion : ( Package.Name, Version ) -> Html msg
viewDepVersion ( name, version ) =
    div [ style "white-space" "nowrap" ]
        [ a
            [ href <|
                Url.absolute
                    [ "packages"
                    , Package.toString name
                    , Version.toString version
                    ]
                    []
            ]
            [ text (Package.toString name ++ " " ++ Version.toString version)
            ]
        ]
