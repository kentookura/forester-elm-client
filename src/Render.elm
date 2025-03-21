module Render exposing
    ( Scope
    , renderArticle
    , renderContent
    , renderContentNode
    , renderFrontmatter
    , renderPrim
      -- , renderSection
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
        , FrontmatterOverrides
        , Img(..)
        , Prim(..)
        , SectionFlags
        , Section_
        , TeXCs_(..)
        , Transclusion
        , article
        , defaultSectionFlags
        , emptyFrontmatterOverrides
        , frontmatter
        )
import Html as H exposing (Html, wbr)
import Html.Attributes as A
import Http exposing (Error(..))
import KaTeX as K
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData(..))


type alias Scope =
    Dict String (Article Content)



-- FIXME: Should not have to reimplement in elm!


applyOverrides : FrontmatterOverrides content -> Frontmatter content -> Frontmatter content
applyOverrides overrides frontmatter =
    { frontmatter
        | title = withDefault frontmatter.title overrides.title
        , taxon = withDefault frontmatter.taxon overrides.taxon
    }



-- FIXME: Should not have to reimplement in elm!


articleToSection :
    Maybe SectionFlags
    -> Maybe (FrontmatterOverrides content)
    -> Article content
    -> Section_ content
articleToSection flags overrides { frontmatter, mainmatter } =
    let
        flgs =
            withDefault defaultSectionFlags flags

        ovrs =
            withDefault emptyFrontmatterOverrides overrides
    in
    { frontmatter = applyOverrides ovrs frontmatter
    , mainmatter = mainmatter
    , flags = flgs
    }



-- FIXME: Should not have to reimplement in elm!


getExpandedTitle : Maybe Addr -> Frontmatter content -> content
getExpandedTitle scope frontmatter =
    let
        shortTitle =
            frontmatter.title
    in
    case frontmatter.designated_parent of
        Just (UserAddr parentAddr) ->
            if scope == frontmatter.designated_parent then
                shortTitle

            else
                shortTitle

        _ ->
            shortTitle



-- FIXME: Should not have to reimplement in elm!


getContentOfTransclusion : Scope -> Transclusion Content -> Content
getContentOfTransclusion scope transclusion =
    case transclusion.target of
        Full flags overrides ->
            case Dict.get (ppAddr transclusion.addr) scope of
                Nothing ->
                    Content []

                Just article ->
                    Content
                        [ Section
                            (articleToSection
                                (Just flags)
                                (Just overrides)
                                article
                            )
                        ]

        Mainmatter ->
            case Dict.get (ppAddr transclusion.addr) scope of
                Nothing ->
                    Content []

                Just article ->
                    article.mainmatter

        Title ->
            case Dict.get (ppAddr transclusion.addr) scope of
                Nothing ->
                    Content []

                Just article ->
                    getExpandedTitle Nothing article.frontmatter

        Taxon ->
            case Dict.get (ppAddr transclusion.addr) scope of
                Nothing ->
                    Content []

                Just article ->
                    let
                        taxon =
                            withDefault "§" article.frontmatter.taxon
                    in
                    Content [ Text taxon ]


renderToc : Scope -> Content -> Html msg
renderToc scope (Content content) =
    let
        tocItem node =
            let
                item { href, title } =
                    H.li
                        []
                        [ H.a
                            [ A.class "bullet", A.href "", A.title "" ]
                            [ H.text "■" ]
                        , H.span
                            [ A.class "link local", A.attribute "data-target" "" ]
                            title
                        ]
            in
            case node of
                Transclude { addr } ->
                    let
                        title =
                            case Dict.get (ppAddr addr) scope of
                                Just article ->
                                    renderContent scope article.frontmatter.title

                                Nothing ->
                                    [ H.text "" ]
                    in
                    Just
                        (item
                            { href = ppAddr addr
                            , title = title
                            }
                        )

                Section { frontmatter } ->
                    Just
                        (item
                            { href = ppAddr frontmatter.addr
                            , title = renderContent scope frontmatter.title
                            }
                        )

                _ ->
                    Nothing
    in
    H.ul [ A.class "block" ] (List.filterMap tocItem content)


renderArticle : Scope -> Article Content -> Html msg
renderArticle scope article =
    H.div [ A.id "grid-wrapper" ]
        [ H.article []
            [ renderMainmatter
                scope
                article
            ]
        , H.nav
            [ A.id "toc" ]
            [ H.div
                [ A.class "block" ]
                [ H.h1
                    []
                    [ H.text "Table of Contents" ]
                , renderToc scope article.mainmatter
                ]
            ]
        ]



-- TODO: Don't use ppAddr


renderFrontmatter : Scope -> Frontmatter Content -> Html msg
renderFrontmatter scope { addr, title } =
    H.header []
        [ H.h1 [] <|
            List.append
                (renderContent scope title)
                [ H.text " "
                , H.a
                    [ A.class "slug", A.href (ppAddr addr) ]
                    [ H.text <| "[" ++ ppAddr addr ++ "]" ]
                ]
        ]



-- TODO: Handle flags


renderMainmatter :
    Scope
    -> { a | frontmatter : Frontmatter Content, mainmatter : Content }
    -> Html msg
renderMainmatter scope { frontmatter, mainmatter } =
    let
        details =
            H.details [ A.attribute "open" "" ]
                [ H.summary []
                    [ renderFrontmatter scope frontmatter ]
                , H.div
                    [ A.class "tree-content" ]
                    (renderContent scope mainmatter)
                ]
    in
    H.section [ A.class "block" ]
        [ details
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


renderTexCS : TeXCs_ -> String
renderTexCS cs =
    case cs of
        Word w ->
            w

        Symbol s ->
            String.fromChar s


renderPlaintextContentNode : Scope -> ContentNode -> String
renderPlaintextContentNode scope n =
    case n of
        Text txt ->
            txt

        CDATA txt ->
            txt

        XmlElt elt ->
            renderPlaintextContent scope elt.content

        Transclude trn ->
            getContentOfTransclusion scope trn |> renderPlaintextContent scope

        ContextualNumber _ ->
            ""

        Section _ ->
            ""

        Prim _ ->
            ""

        KaTeX _ content ->
            renderPlaintextContent scope content

        TeXCs cs ->
            "\\" ++ renderTexCS cs

        Link _ ->
            ""

        Resource _ ->
            ""

        Img _ ->
            ""

        ResultsOfQuery _ ->
            ""


renderPlaintextContent : Scope -> Content -> String
renderPlaintextContent scope (Content ns) =
    String.concat (List.map (\n -> renderPlaintextContentNode scope n) ns)


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
                    [ H.text <| "fetching tree at address " ++ ppAddr addr ]

                Just article ->
                    case target of
                        Full flags overrides ->
                            [ renderMainmatter scope article ]

                        Mainmatter ->
                            -- renderContent scope article.mainmatter
                            [ renderMainmatter scope article ]

                        Title ->
                            renderContent scope article.frontmatter.title

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

        Section { frontmatter, mainmatter, flags } ->
            [ renderMainmatter scope { frontmatter = frontmatter, mainmatter = mainmatter } ]

        Prim ( p, c ) ->
            [ renderPrim p <| renderContent scope c ]

        KaTeX mode markup ->
            let
                m =
                    renderPlaintextContent scope markup
            in
            [ K.view { display = mode, markup = m }
            ]

        TeXCs texcs ->
            case texcs of
                Word _ ->
                    []

                Symbol _ ->
                    []

        Link { href, content } ->
            [ H.a [ A.class "slug", A.href href ] <| renderContent scope content ]

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


renderPrim : a -> List (Html msg) -> Html msg
renderPrim _ =
    tagOfPrim P []


tagOfPrim : Prim -> List (H.Attribute msg) -> List (Html msg) -> Html msg
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
