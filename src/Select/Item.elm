module Select.Item exposing (..)


type alias Item a =
    { id : String
    , label : String
    , identity : a
    , isSelected : Bool
    , isSelectable : Bool
    }


type Items a
    = List (Item a)
