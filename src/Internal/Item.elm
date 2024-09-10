module Internal.Item exposing (Item, Items)


type alias Items item =
    List (Item item)


type alias Item item =
    { id : String
    , label : String
    , identity : item
    , isSelectable : Bool
    }
