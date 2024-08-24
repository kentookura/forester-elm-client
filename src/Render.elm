module Render exposing
    ( renderArticle
    , renderContent
    , renderContentNode
    , renderFrontmatter
    , renderPrim
    , renderSection
    , renderXmlQname
    , tagOfPrim
    )

import Forester.Base exposing (Addr(..), XmlQname)
import Forester.Query exposing (Expr(..))
import Forester.XmlTree
    exposing
        ( Article
        , Content(..)
        , ContentNode(..)
        , Frontmatter
        , Img(..)
        , Prim(..)
        , Section_
        , TeXCs_(..)
        , article
        )
import Html as H exposing (Html, div)
import Html.Attributes as A
import Http exposing (Error(..))
import KaTeX as K
import RemoteData exposing (RemoteData(..))


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

        KaTeX mode _ ->
            [ K.view { display = mode, markup = "a=b" } ]

        TeXCs texcs ->
            case texcs of
                Word _ ->
                    []

                Symbol _ ->
                    []

        Link _ ->
            []

        Img img ->
            case img of
                Inline { format, base64 } ->
                    [ H.img [ A.src <| "data:image/" ++ format ++ ";base64," ++ base64 ] [] ]

                Remote url ->
                    [ H.img [ A.src url ] [] ]

        Resource _ ->
            []


renderPrim _ =
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
