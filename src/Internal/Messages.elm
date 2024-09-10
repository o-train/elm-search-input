module Internal.Messages exposing (Msg)

import Browser.Dom
import Debouncer.Basic as Debouncer
import Internal.Setters exposing (Setters)


type Msg item
    = Autocomplete
    | Set (Setters item)
    | FocusInput (Result Browser.Dom.Error ())
    | RunSearch
    | QueueSearch (Debouncer.Msg (Msg item))
    | NoOperation
