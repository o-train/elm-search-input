module Internal.Setters exposing (Direction, OpenState, Setters)

import Internal.Filter exposing (Filter)
import Internal.Item exposing (Item)
import RemoteData exposing (WebData)


type Direction
    = Next
    | Previous


type OpenState
    = Opened
    | Closed


type Setters item
    = Mode OpenState
    | Select (Item item)
    | Deselect (Item item)
    | AutocompleteIndex Direction
    | SearchTerms String
    | SuggestedItems
    | SearchResults (WebData (List (Item item)))
    | Filter Filter
