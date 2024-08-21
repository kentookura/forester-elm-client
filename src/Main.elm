module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Forester.XmlTree exposing (Article, Content(..), ContentNode(..), Frontmatter, Prim(..), Section_, article)
import Html as H exposing (Html, div, pre, text)
import Html.Attributes as A
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


renderContentNode node =
    case node of
        Text str ->
            [ H.text str ]

        CDATA str ->
            [ H.text <| "<!CDATA[" ++ str ++ ">" ]

        XmlElt elt ->
            []

        Transclude _ ->
            []

        ResultsOfQuery _ ->
            []

        Section _ ->
            []

        Prim ( p, c ) ->
            [ renderPrim p <| renderContent c ]

        KaTeX _ _ ->
            []

        TeXCs _ ->
            []

        Link _ ->
            []

        Img _ ->
            []

        Resource _ ->
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



{--

render_content_node : T.content_node -> P.node list =
function
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw:true "<![CDATA[%s]]>" str]
    | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        Xmlns.within_scope @@ fun () ->
        render_xml_qname elt.name,
        List.map render_xml_attr elt.attrs,
        render_content elt.content
      in
      let attrs =
        let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
        attrs @ xmlns_attrs
      in
      [P.std_tag name attrs content]

    | Prim (p, content) ->
      [render_prim_node p @@ render_content content]
    | Transclude transclusion ->
      render_transclusion transclusion
    | Link link ->
      render_link link
    | Results_of_query q ->
      F.run_query q
      |> Util.get_sorted_articles
      |> List.map (Fun.compose render_section T.article_to_section)
    | Section section ->
      [render_section section]
    | KaTeX (mode, content) ->
      let l, r =
        match mode with
        | Display -> {|\[|}, {|\]|}
        | Inline -> {|\(|}, {|\)|}
      in
      let body = PT.string_of_content content in
      [P.txt ~raw:true "%s%s%s" l body r]
    | TeX_cs cs ->
      [P.txt ~raw:true "\\%s" (TeX_cs.show cs)]
    | Img img ->
      [render_img img]
    | Resource resource ->
      render_resource resource

   render_resource resource =
    render_content resource.content
    --}
-- div [] [ article |> Debug.toString |> text ]


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
