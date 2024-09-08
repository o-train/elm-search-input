module Internal.Filter exposing (Filter, Filters)


type Filter
    = Checkbox Bool
    | TextBox String


type alias Filters =
    List Filter
