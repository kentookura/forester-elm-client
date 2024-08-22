module KaTeX exposing (..)

import Forester.Base exposing (MathMode(..))
import Html exposing (Html, node)
import Html.Attributes exposing (attribute, id)


type alias KaTeX =
    { markup : String
    , display : MathMode
    }


katex : String -> KaTeX
katex markup =
    { markup = markup, display = Display }


withDisplay : MathMode -> KaTeX -> KaTeX
withDisplay display k =
    { k | display = display }


asInline : KaTeX -> KaTeX
asInline k =
    { k | display = Inline }


asDisplay : KaTeX -> KaTeX
asDisplay k =
    { k | display = Display }


view : KaTeX -> Html msg
view k =
    let
        displayToString d =
            case d of
                Inline ->
                    "inline"

                Display ->
                    "block"
    in
    node "embed-katex"
        [ id "embed-katex"
        , attribute "markup" k.markup
        , attribute "display" (displayToString k.display)
        ]
        []
