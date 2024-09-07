module Icons.Search exposing (view, viewWithAttributes)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Html msg
view =
    viewWithAttributes []


viewWithAttributes : List (Html.Attribute msg) -> Html msg
viewWithAttributes attributes =
    svg
        ([ height "100%", viewBox "0 0 16 18", version "1.1" ] ++ attributes)
        [ Svg.g
            [ id "Page-1"
            , strokeWidth "1"
            , stroke "currentColor"
            , fill "none"
            , fillRule "evenodd"
            ]
            [ Svg.g
                [ transform "translate(-19.000000, -57.000000)", strokeWidth "2" ]
                [ Svg.g
                    [ id "Search", transform "translate(0.000000, 47.000000)" ]
                    [ Svg.g
                        [ id "Group-Copy", transform "translate(28.093999, 19.282840) scale(-1, 1) rotate(80.000000) translate(-28.093999, -19.282840) translate(21.093999, 11.282840)" ]
                        [ Svg.circle
                            [ id "Oval-3", fill "", cx "5.5", cy "5.5", r "5.5" ]
                            []
                        , Svg.path [ d "M9.05664062,9.82421875 L13.4472656,15.0605469", id "Path-3" ] []
                        ]
                    ]
                ]
            ]
        ]
