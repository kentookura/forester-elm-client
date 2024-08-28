module Render exposing
    ( Scope
    , renderArticle
    , renderContent
    , renderContentNode
    , renderFrontmatter
    , renderPrim
    , renderSection
    , renderXmlQname
    , tagOfPrim
    )

import Dict exposing (Dict)
import Forester.Base exposing (Addr(..), XmlQname, ppAddr)
import Forester.Query exposing (Expr(..))
import Forester.XmlTree
    exposing
        ( Article
        , Content(..)
        , ContentNode(..)
        , ContentTarget(..)
        , Frontmatter
        , Img(..)
        , Prim(..)
        , Section_
        , TeXCs_(..)
        , article
        )
import Html as H exposing (Html, a, div)
import Html.Attributes as A
import Html.Events as E
import Http exposing (Error(..))
import KaTeX as K
import RemoteData exposing (RemoteData(..))


type alias Scope =
    Dict String (Article Content)


renderArticle : Scope -> Article Content -> Html msg
renderArticle scope article =
    H.article []
        [ renderFrontmatter scope article.frontmatter
        , div [] (renderContent scope article.mainmatter)
        , H.section [ A.class "backmatter" ]
            (renderContent
                scope
                article.backmatter
            )
        ]


renderSection : Scope -> Section_ Content -> Html msg
renderSection scope section =
    H.section []
        [ renderFrontmatter scope section.frontmatter
        , div [] <| renderContent scope section.mainmatter
        ]


renderFrontmatter : Scope -> Frontmatter Content -> Html msg
renderFrontmatter scope frontmatter =
    H.header []
        [ H.h1 [] <| renderContent scope frontmatter.title
        ]


renderContent :
    Scope
    -> Content
    -> List (Html msg)
renderContent scope (Content content) =
    List.concat
        (List.map (\c -> renderContentNode scope c) content)


renderXmlQname : XmlQname -> String
renderXmlQname qname =
    case qname of
        { prefix, uname } ->
            if prefix == "" then
                uname

            else
                prefix ++ ":" ++ uname


renderContentNode :
    Scope
    -> ContentNode
    -> List (Html msg)
renderContentNode scope node =
    case node of
        Text str ->
            [ H.text str ]

        CDATA str ->
            [ H.text <| "<!CDATA[" ++ str ++ ">" ]

        XmlElt elt ->
            -- TODO: hmmm
            [ H.node (renderXmlQname elt.name) [] [] ]

        Transclude { addr, target, modifier } ->
            case Dict.get (ppAddr addr) scope of
                Nothing ->
                    [ H.text <| "fetching tree at address" ++ ppAddr addr ]

                Just article ->
                    case target of
                        Full flags overrides ->
                            [ renderArticle scope article ]

                        Mainmatter ->
                            [ renderArticle scope article ]

                        Title ->
                            [ renderArticle scope article ]

                        Taxon ->
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
            [ renderSection scope content ]

        Prim ( p, c ) ->
            [ renderPrim p <| renderContent scope c ]

        KaTeX mode markup ->
            [ K.view { display = mode, markup = "a=b" }
            ]

        TeXCs texcs ->
            case texcs of
                Word _ ->
                    []

                Symbol _ ->
                    []

        Link { href, content } ->
            [ a [ A.href href ] <| renderContent scope content ]

        Img img ->
            case img of
                Inline { format, base64 } ->
                    [ H.img [ A.src <| "data:image/" ++ format ++ ";base64," ++ base64 ] [] ]

                Remote url ->
                    [ H.img [ A.src url ] [] ]

        ContextualNumber addr ->
            let
                customNumber =
                    Dict.get (ppAddr addr) scope
                        |> Maybe.andThen
                            (\article ->
                                article.frontmatter.number
                            )
                        |> (\m ->
                                case m of
                                    Nothing ->
                                        "[" ++ ppAddr addr ++ "]"

                                    Just num ->
                                        num
                           )
            in
            [ H.text customNumber ]

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
