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
import Html exposing (pre, text)
import Http exposing (Error(..), expectJson, jsonBody)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (RemoteData(..), WebData)
import Render exposing (Scope, renderArticle)
import Url


type alias Model =
    { fragments : Scope
    , queryResults : Dict String (List Addr)
    , config : WebData Config
    , key : Nav.Key
    , url : Url.Url
    }


type Msg
    = ArticleResponse (WebData (Article Content))
      -- We need to keep track of which query was run. If we encounter just a
      -- WebData (List Addr), we can't really do anything with it
    | QueryResponse (Query.Expr Query.Dbix) (WebData (List Addr))
    | ConfigResponse (WebData Config)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


apiURL : String
apiURL =
    "http://localhost:8000/"


getArticle : Addr -> Cmd Msg
getArticle addr =
    Http.get
        { url = apiURL ++ "trees/" ++ ppAddr addr
        , expect =
            expectJson
                (RemoteData.fromResult >> ArticleResponse)
                article
        }


getConfig : Cmd Msg
getConfig =
    Http.get
        { url = apiURL ++ "config"
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
        { url = apiURL ++ "query"
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
    | L String


hypermediaControl : ContentNode -> List HypermediaControl
hypermediaControl n =
    case n of
        Transclude transclusion ->
            [ T transclusion ]

        ResultsOfQuery expr ->
            [ Q expr ]

        Link { href } ->
            [ L href ]

        XmlElt { content } ->
            let
                (Content c) =
                    content
            in
            List.concat (List.map hypermediaControl c)

        Section { frontmatter, mainmatter } ->
            let
                (Content mm) =
                    mainmatter

                -- TODO: handle metas
                { title, metas } =
                    frontmatter

                ts =
                    case title of
                        Content c ->
                            List.concat (List.map hypermediaControl c)
            in
            List.append ts <| List.concat (List.map hypermediaControl mm)

        Prim ( _, Content content ) ->
            List.concat <| List.map hypermediaControl content

        KaTeX _ (Content content) ->
            List.concat <| List.map hypermediaControl content

        Text _ ->
            []

        CDATA _ ->
            []

        TeXCs _ ->
            []

        Img _ ->
            []

        Resource _ ->
            []


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { fragments = Dict.empty
      , queryResults = Dict.empty
      , config = NotAsked
      , key = key
      , url = url
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
                cmd =
                    case c of
                        Success config ->
                            getArticle (UserAddr config.root)

                        _ ->
                            Cmd.none
            in
            ( { model | config = c }, cmd )

        ArticleResponse response ->
            case response of
                Success article ->
                    ( { model
                        | fragments =
                            Dict.insert (article.frontmatter.addr |> ppAddr) article model.fragments
                      }
                    , let
                        (Content nodes) =
                            article.mainmatter
                      in
                      nodes
                        |> List.concatMap hypermediaControl
                        |> List.map
                            (\c ->
                                case c of
                                    T { addr } ->
                                        getArticle addr

                                    Q query ->
                                        runQuery query

                                    L href ->
                                        getArticle (UserAddr href)
                            )
                        |> Cmd.batch
                    )

                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )


view : Model -> Document Msg
view model =
    let
        body =
            let
                scope =
                    model.fragments
            in
            case model.config of
                Failure _ ->
                    pre [] [ text "could not load config. Is the backend running?" ]

                NotAsked ->
                    pre [] [ text "waiting" ]

                Loading ->
                    pre [] [ text "loading config..." ]

                Success config ->
                    case model.url.path of
                        "/" ->
                            case Dict.get config.root scope of
                                Just article ->
                                    renderArticle scope article

                                Nothing ->
                                    pre [] [ text "no root" ]

                        addr ->
                            -- TODO: Actually parse the path.
                            case Dict.get (String.dropLeft 1 addr) scope of
                                Just article ->
                                    renderArticle scope article

                                Nothing ->
                                    pre [] [ text <| "Internal error: could not find address " ++ addr ++ " in scope" ]
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
