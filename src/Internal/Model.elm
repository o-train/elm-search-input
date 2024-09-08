module Internal.Model exposing (Model, OpenState)

import Debouncer.Basic as Debouncer
import Internal.Item exposing (Item)
import Internal.Messages exposing (Msg)
import Internal.Search exposing (Search)
import RemoteData exposing (WebData)


type OpenState
    = Opened
    | Closed


type alias Model item =
    { label : String
    , selected : Maybe (Item item)

    -- Search/Mappers
    -- Should be a Msg?
    , toItem : item -> Item item
    , runSearch : Search item -> ( Search item, Maybe (Msg item) )

    --  Config
    , search : Search item

    --  Internal Config
    , mode : OpenState
    , searchDebouncer : Debouncer.Debouncer (Msg item) (Msg item)
    , autocompleteIndex : Maybe Int

    --  Advanced Config
    , allowMultipleFilters : Bool
    , minimumSearchTermsLength : Int
    }
