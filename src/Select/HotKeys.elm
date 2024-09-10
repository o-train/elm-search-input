module Select.HotKeys exposing (msgForKeyboardEvent)

import Select.Item exposing (Item, Items)


msgForKeyboardEvent : Items -> Maybe Int -> List ( String, Msg )
msgForKeyboardEvent records autocompleteIndex =
    let
        { onFirst, onLast, noAutocompleteIndex } =
            autocompleteIndexDetails autocompleteIndex False records

        downOp =
            if not onLast || noAutocompleteIndex then
                Set (AutocompleteIndex Next)

            else
                NoOperation

        upOp =
            if not onFirst then
                Set (AutocompleteIndex Previous)

            else
                NoOperation
    in
    [ ( "Escape", Close )
    , ( "Enter", Autocomplete )
    , ( "Tab", Autocomplete )
    , ( "ArrowDown", downOp )
    , ( "ArrowUp", upOp )
    ]


autocompleteIndexDetails : Maybe Int -> Bool -> Items -> AutocompletePositionalDetails
autocompleteIndexDetails currentIndex addOneForPendingMatter records =
    let
        autocompleteListLength =
            records
                |> List.length
                |> (\n ->
                        if addOneForPendingMatter then
                            n + 1

                        else
                            n
                   )

        onLast =
            currentIndex == Just (autocompleteListLength - 1)

        onFirst =
            currentIndex == Just 0

        noAutocomplete =
            Maybe.Extra.isNothing currentIndex
    in
    { onFirst = onFirst
    , onLast = onLast
    , noAutocompleteIndex = noAutocomplete
    }


type alias AutocompletePositionalDetails =
    { onFirst : Bool, onLast : Bool, noAutocompleteIndex : Bool }
