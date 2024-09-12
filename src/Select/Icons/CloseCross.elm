module Select.Icons.CloseCross exposing (view, viewWithAttributes)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Html msg
view =
    viewWithAttributes []


viewWithAttributes : List (Html.Attribute msg) -> Html msg
viewWithAttributes attributes =
    svg
        ([ height "100%", viewBox "0 0 24 24", version "1.1", stroke "currentColor" ] ++ attributes)
        [ Svg.g
            [ strokeWidth "1", fill "none", fillRule "evenodd", strokeLinecap "round", strokeLinejoin "round" ]
            [ Svg.g [ id "cross-copy-3", transform "translate(-36.000000, -36.000000)", strokeWidth "3" ] [ Svg.g [ id "close-cross", transform "translate(38.000000, 38.000000)" ] [ Svg.path [ d "M20,0 L0,20", id "Path-2" ] [], Svg.path [ d "M0,0 L20,20", id "Path-1" ] [] ] ] ]
        ]
