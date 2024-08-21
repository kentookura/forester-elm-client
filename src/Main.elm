module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Forester.XmlTree exposing (Article, Content, article)
import Html exposing (Html, div, pre, text)
import Http exposing (expectJson)
import RemoteData exposing (RemoteData(..), WebData)
import Url


type alias Model =
    { article : WebData (Article Content) }


type Msg
    = ArticleResponse (WebData (Article Content))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


getarticle : Cmd Msg
getarticle =
    Http.get
        { url = "http://localhost:8000"
        , expect =
            expectJson
                (RemoteData.fromResult >> ArticleResponse)
                article
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { article = Loading }, getarticle )


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
                    pre [] [ text ("Error: " ++ toString err) ]

                Success article ->
                    viewArticle article
    in
    { title = "forester", body = [ body ] }


viewArticle : Article Content -> Html msg
viewArticle article =
    div [] [ article |> Debug.toString |> text ]


subscriptions : Model -> Sub Msg
subscriptions model =
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
