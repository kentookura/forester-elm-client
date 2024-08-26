module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Forester.Base exposing (Addr(..), MathMode(..), addr, ppAddr)
import Forester.Config exposing (Config, config)
import Forester.Query as Query exposing (Expr(..), encodeExpr)
import Forester.Query.Hash
import Forester.XmlTree
    exposing
        ( Article
        , Content(..)
        , ContentNode(..)
        , Img(..)
        , Link_
        , Prim(..)
        , TeXCs_(..)
        , Transclusion
        , article
        )
import Hash
import Html exposing (pre)
import Http exposing (Error(..), expectJson, jsonBody)
import Json.Decode as Decode
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (RemoteData(..), WebData)
import Render exposing (Scope, renderArticle)
import Url


type alias Model =
    { fragments : Scope
    , queryResults : Dict String (List Addr)
    , root : Maybe Addr
    , buffer : String
    , config : WebData Config
    }


type Msg
    = ArticleResponse (WebData (Article Content))
    | ContentResponse (WebData Content)
      -- we need to keep track of which query was run. If we encounter just a
      -- WebData (List Addr), we can't really do anything with it
    | QueryResponse (Query.Expr Query.Dbix) (WebData (List Addr))
    | ConfigResponse (WebData Config)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


getArticle : Addr -> Cmd Msg
getArticle addr =
    Http.get
        { url = "http://localhost:8000/" ++ ppAddr addr
        , expect =
            expectJson
                (RemoteData.fromResult >> ArticleResponse)
                article
        }


getConfig : Cmd Msg
getConfig =
    Http.get
        { url = "http://localhost:8000/config"
        , expect =
            expectJson
                (RemoteData.fromResult >> ConfigResponse)
                config
        }


runQuery : Query.Expr Query.Dbix -> Cmd Msg
runQuery query =
    let
        encodeDbix i =
            Encode.int i
    in
    Http.post
        { url = "http://localhost:8000/query"
        , body = jsonBody (encodeExpr encodeDbix query)
        , expect =
            Http.expectJson
                (\res ->
                    RemoteData.fromResult res
                        |> (\rd -> QueryResponse query rd)
                )
                (Decode.list addr)
        }


type HypermediaControl
    = T (Transclusion Content)
    | Q (Query.Expr Int)
    | L (Link_ Content)


hypermediaControl : ContentNode -> Maybe HypermediaControl
hypermediaControl n =
    case n of
        Transclude transclusion ->
            Just (T transclusion)

        ResultsOfQuery expr ->
            Just (Q expr)

        Link link ->
            Just (L link)

        _ ->
            Nothing


getDependents : Content -> Cmd Msg
getDependents (Content nodes) =
    nodes
        |> List.filterMap hypermediaControl
        |> List.map
            (\c ->
                case c of
                    T { addr, target, modifier } ->
                        getArticle addr

                    Q query ->
                        runQuery query

                    L { href, content } ->
                        Cmd.none
            )
        |> Cmd.batch


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { fragments = Dict.empty
      , buffer = ""
      , queryResults = Dict.empty
      , config = NotAsked
      , root = Nothing
      }
    , getConfig
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryResponse query response ->
            case response of
                Success trees ->
                    let
                        queryHash =
                            Forester.Query.Hash.hashQuery query |> Hash.toString
                    in
                    ( { model
                        | queryResults =
                            Dict.insert
                                queryHash
                                trees
                                model.queryResults
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ConfigResponse c ->
            let
                ( cmd, root ) =
                    case c of
                        Success config ->
                            ( getArticle (UserAddr config.root), Just (UserAddr config.root) )

                        _ ->
                            ( Cmd.none, Nothing )
            in
            ( { model | config = c, root = root }, cmd )

        ContentResponse response ->
            case response of
                Success content ->
                    ( model, Cmd.none )

                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )

        ArticleResponse response ->
            case response of
                Success article ->
                    ( { model
                        | fragments =
                            Dict.insert (article.frontmatter.addr |> ppAddr) article model.fragments
                      }
                    , getDependents article.mainmatter
                    )

                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        body =
            let
                scope =
                    model.fragments
            in
            case model.root of
                Just addr ->
                    case Dict.get (ppAddr addr) model.fragments of
                        Just article ->
                            renderArticle scope article

                        Nothing ->
                            pre [] []

                Nothing ->
                    pre [] []
    in
    { title = "forester", body = [ body ] }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
