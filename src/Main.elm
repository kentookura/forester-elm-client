module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Forester.Base exposing (Addr(..), XmlQname, pp_addr)
import Forester.Query exposing (Expr(..))
import Forester.XmlTree exposing (Article, Content(..), ContentNode(..), Frontmatter, Img(..), Prim(..), Section_, TeXCs_(..), article)
import Html as H exposing (Html, div, pre, text)
import Html.Attributes as A
import Http exposing (Error(..), expectJson)
import KaTeX as K
import RemoteData exposing (RemoteData(..), WebData)
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
init _ url key =
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


renderArticle : Article Content -> Html msg
renderArticle article =
    H.article []
        [ renderFrontmatter article.frontmatter
        , div [] (renderContent article.mainmatter)
        , H.section [ A.class "backmatter" ] (renderContent article.backmatter)
        ]


renderSection : Section_ Content -> Html msg
renderSection section =
    H.section []
        [ renderFrontmatter section.frontmatter
        , div [] <| renderContent section.mainmatter
        ]


renderFrontmatter : Frontmatter Content -> Html msg
renderFrontmatter frontmatter =
    H.header []
        [ H.h1 [] <| renderContent frontmatter.title
        ]


renderContent : Content -> List (Html msg)
renderContent (Content content) =
    List.concat
        (List.map renderContentNode content)


renderXmlQname : XmlQname -> String
renderXmlQname qname =
    case qname of
        { prefix, uname } ->
            if prefix == "" then
                uname

            else
                prefix ++ ":" ++ uname


renderContentNode node =
    case node of
        Text str ->
            [ H.text str ]

        CDATA str ->
            [ H.text <| "<!CDATA[" ++ str ++ ">" ]

        XmlElt elt ->
            -- TODO: hmmm
            [ H.node (renderXmlQname elt.name) [] [] ]

        Transclude _ ->
            []

        ResultsOfQuery expr ->
            case expr of
                Rel _ _ _ _ ->
                    []

                Isect _ ->
                    []

                Union _ ->
                    []

                Complement _ ->
                    []

                UnionFam _ _ ->
                    []

                IsectFam _ _ ->
                    []

        Section content ->
            [ renderSection content ]

        Prim ( p, c ) ->
            [ renderPrim p <| renderContent c ]

        KaTeX mode content ->
            [ K.view { display = mode, markup = "a=b" } ]

        TeXCs texcs ->
            case texcs of
                Word str ->
                    []

                Symbol str ->
                    []

        Link { href, content } ->
            []

        Img img ->
            case img of
                Inline { format, base64 } ->
                    [ H.img [ A.src <| "data:image/" ++ format ++ ";base64," ++ base64 ] [] ]

                Remote url ->
                    [ H.img [ A.src url ] [] ]

        Resource { hash, content, sources } ->
            []


renderPrim p =
    tagOfPrim P []


tagOfPrim p =
    case p of
        P ->
            H.p

        Em ->
            H.em

        Strong ->
            H.strong

        Figure ->
            H.figure

        Figcaption ->
            H.figcaption

        Ul ->
            H.ul

        Ol ->
            H.ol

        Li ->
            H.li

        Blockquote ->
            H.blockquote

        Code ->
            H.code

        Pre ->
            H.pre


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
