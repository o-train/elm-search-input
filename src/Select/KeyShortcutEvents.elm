module Select.KeyShortcutEvents exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Events
import Keyboard.Event exposing (KeyboardEvent)


getInDict : Dict comparable v -> comparable -> Maybe v
getInDict dict key =
    Dict.get key dict


keyMapper : List ( String, msg ) -> KeyboardEvent -> Maybe ( msg, Bool )
keyMapper specAsList keyboardEvent =
    let
        spec =
            Dict.fromList specAsList
    in
    keyboardEvent.key
        |> Maybe.andThen (getInDict spec)
        |> Maybe.map
            (\msg ->
                ( msg
                , case keyboardEvent.key of
                    -- Stops propagation so input/pop up menus close without closing fee
                    Just "Escape" ->
                        True

                    _ ->
                        False
                )
            )


{-| Handle keyshortcut events from a spec -- a list of tuples [ ( key, msg ) ]

    Will stop propagation of all key presses, as this see

-}
on : List ( String, msg ) -> Attribute msg
on specAsList =
    Html.Events.stopPropagationOn "keydown"
        (Keyboard.Event.considerKeyboardEvent
            (keyMapper specAsList)
        )
