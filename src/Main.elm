module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Forester.Base exposing (Addr(..), MathMode(..), ppAddr)
import Forester.Config exposing (Config, config)
import Forester.Query as Query exposing (Expr(..))
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
        , content
        )
import Html exposing (div, pre, text, wbr)
import Http exposing (Error(..), expectJson)
import KaTeX as K
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (RemoteData(..), WebData)
import Render exposing (renderArticle)
import Url


type alias Model =
    { fragments : Dict String (Article Content)
    , root : Maybe Addr
    , buffer : String
    , config : WebData Config
    }


type Msg
    = ArticleResponse (WebData (Article Content))
    | ContentResponse (WebData Content)
    | ConfigResponse (WebData Config)
    | HypermediaRequest HypermediaControl
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


getContent : Addr -> Cmd Msg
getContent addr =
    Http.get
        { url = "http://localhost:8000/" ++ ppAddr addr
        , expect =
            expectJson
                (RemoteData.fromResult >> ContentResponse)
                content
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
                        Cmd.none

                    L { href, content } ->
                        Cmd.none
            )
        |> Cmd.batch


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { fragments = Dict.empty
      , buffer = ""
      , config = NotAsked
      , root = Nothing
      }
    , getConfig
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HypermediaRequest req ->
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


view : Model -> Document msg
view model =
    let
        body =
            case model.root of
                Just addr ->
                    case Dict.get (ppAddr addr) model.fragments of
                        Just article ->
                            renderArticle article

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
