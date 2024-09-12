module Select.AutocompleteLogic exposing (..)

import List.Extra
import Select.Autocomplete exposing (Direction(..))


changeStep : Direction -> Maybe Int -> Maybe Int
changeStep dir index =
    case ( dir, index ) of
        ( Next, Nothing ) ->
            Just 0

        ( Next, Just i ) ->
            Just (i + 1)

        ( Previous, Nothing ) ->
            Nothing

        ( Previous, Just i ) ->
            Just (i - 1)
