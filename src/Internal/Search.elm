module Internal.Search exposing (Filter, Filters, Search)

import Internal.Item exposing (Item, Items)
import RemoteData exposing (WebData)


type Filter
    = Checkbox Bool
    | TextBox String


type alias Filters =
    List Filter


type alias Search item =
    { filters : Filters
    , terms : Maybe String
    , items : List (Item item)
    , results : WebData (Items item)
    }
