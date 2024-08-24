module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Forester.Base exposing (Addr(..), pp_addr)
import Forester.Query exposing (Expr(..))
import Forester.XmlTree
    exposing
        ( Article
        , Content(..)
        , ContentNode(..)
        , Img(..)
        , Prim(..)
        , TeXCs_(..)
        , article
        )
import Html exposing (div, pre, text)
import Http exposing (Error(..), expectJson)
import RemoteData exposing (RemoteData(..), WebData)
import Render exposing (renderArticle)
import Url


type alias Model =
    { article : WebData (Article Content), buffer : String }


type Msg
    = ArticleResponse (WebData (Article Content))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


getArticle : Addr -> Cmd Msg
getArticle addr =
    Http.get
        { url = "http://localhost:8000/" ++ pp_addr addr
        , expect =
            expectJson
                (RemoteData.fromResult >> ArticleResponse)
                article
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { article = Loading, buffer = "" }, getArticle (UserAddr "hello") )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ArticleResponse response ->
            ( { model | article = response }, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


view : Model -> Document msg
view model =
    let
        body =
            case model.article of
                NotAsked ->
                    text "Initialising."

                Loading ->
                    text "Loading"

                Failure err ->
                    case err of
                        BadUrl url ->
                            pre [] [ text <| "Internal error: " ++ url ++ "is not a valid url" ]

                        Timeout ->
                            div [] [ text "request timed out" ]

                        NetworkError ->
                            pre [] [ text "Network error when fetching tree. Is the server running?" ]

                        BadStatus status ->
                            pre [] [ text <| "request returned status " ++ String.fromInt status ]

                        BadBody str ->
                            pre [] [ text str ]

                Success article ->
                    renderArticle article
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
