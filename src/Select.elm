module Select exposing (Item, Model, Msg(..), OpenState(..), Search, Setters(..), basicInit, update, view)

import Browser.Dom
import Debouncer.Basic as Debouncer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onClickStopPropagation, onEnter)
import Html.Keyed
import Icons.CloseCross
import Icons.Search
import RemoteData exposing (WebData)
import Select.KeyShortcutEvents
import Select.OnClickOutsideAfterFocus
import Svg.Attributes
import Task



-- Comment
{- Basic + Advanced -}


type Direction
    = Next
    | Previous


type OpenState
    = Opened
    | Closed


type Filter
    = Checkbox Bool
    | TextBox String


type alias Filters =
    List Filter


type alias Item item =
    { id : String
    , label : String
    , identity : item
    , isSelectable : Bool
    }


type alias Items item =
    List (Item item)


type alias Search item =
    { filters : Filters
    , terms : Maybe String
    , items : List (Item item)
    , results : WebData (Items item)
    }


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


type SearchType
    = Basic
    | Query


type Msg item
    = Autocomplete
    | Set (Setters item)
    | FocusInput (Result Browser.Dom.Error ())
    | RunSearch
    | QueueSearch (Debouncer.Msg (Msg item))
    | NoOperation


type Setters item
    = Mode OpenState
    | Select (Item item)
    | AutocompleteIndex Direction
    | SearchTerms String
    | SuggestedItems
    | SearchResults (WebData (List (Item item)))
    | Filter Filter


updateSearchTerms : Maybe String -> Search item -> Search item
updateSearchTerms terms search =
    { search | terms = terms }


updateSearchResults : WebData (Items item) -> Search item -> Search item
updateSearchResults results search =
    { search | results = results }


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        Set (Mode Closed) ->
            ( { model | mode = Closed, search = updateSearchTerms Nothing model.search }
            , Cmd.none
            )

        Set (Mode Opened) ->
            ( { model | mode = Opened }
            , Cmd.none
            )

        Set (SearchTerms terms) ->
            let
                sanitisedTerms =
                    String.trim terms

                runnable =
                    String.length sanitisedTerms >= model.minimumSearchTermsLength

                _ =
                    Debug.log "Hi " sanitisedTerms

                _ =
                    Debug.log "Runnable " runnable

                cmd =
                    if runnable then
                        RunSearch
                            |> Debouncer.provideInput
                            |> QueueSearch
                            |> sendMsg

                    else
                        Cmd.none

                _ =
                    Debug.log "Set Search Terms + Cmd Msg " cmd

                autocompleteIndex =
                    if String.length sanitisedTerms > 0 then
                        Just 0

                    else
                        Nothing
            in
            ( { model
                | search = model.search |> updateSearchTerms (Just terms) |> updateSearchResults RemoteData.NotAsked
                , autocompleteIndex = autocompleteIndex
              }
            , cmd
            )

        QueueSearch subMsg ->
            let
                ( debouncedModel, subCmd, emittedMsg ) =
                    Debouncer.update subMsg model.searchDebouncer

                mappedCmd =
                    Cmd.map QueueSearch subCmd

                updatedModel =
                    { model | searchDebouncer = debouncedModel }

                _ =
                    Debug.log "QueueSearch" a
            in
            case emittedMsg of
                Just emitted ->
                    let
                        ( m, c ) =
                            update emitted updatedModel
                    in
                    ( m, Cmd.batch [ c, mappedCmd ] )

                Nothing ->
                    ( updatedModel, mappedCmd )

        RunSearch ->
            let
                ( updatedSearch, searchMsg ) =
                    model.search
                        |> model.runSearch
            in
            ( { model | search = updatedSearch }, searchMsg |> Maybe.map sendMsg |> Maybe.withDefault Cmd.none )

        a ->
            let
                _ =
                    Debug.log "What" a
            in
            ( model, Cmd.none )


view : Model a -> Html (Msg a)
view model =
    Html.Keyed.node "div"
        ([ class "overflow-y-scroll z-10 md:pt-3 mb-8 md:mb-0 min-w-full md:min-w-auto"
         , class "sm:max-w-sm md:min-h-80 md:max-h-92 lg:max-w-lg"
         , attribute "data-role" "search-input-view"
         ]
            ++ Select.OnClickOutsideAfterFocus.withId (domId model.label) (Set (Mode Closed))
        )
        [ ( "search-input-" ++ model.label, searchInputView model )
        , ( "search-items", itemsView model.selected model.search.results model.autocompleteIndex )
        ]


itemsView : Maybe (Item item) -> WebData (Items item) -> Maybe Int -> Html (Msg item)
itemsView selected webdataItems autocompleteIndex =
    case webdataItems of
        RemoteData.NotAsked ->
            span [ class "px-4 text-grey" ] [ text "Waiting for at least 2 letters..." ]

        RemoteData.Loading ->
            span [ class "px-4 text-grey" ] [ text "Loading..." ]

        RemoteData.Failure err ->
            span [ class "px-4 text-grey" ] [ text "Search failed..." ]

        RemoteData.Success [] ->
            span [ class "px-4 text-grey" ] [ text "No results matching terms..." ]

        RemoteData.Success items ->
            div
                [ class "max-w-lg"
                , attribute "data-role" "items-list"
                ]
                -- Keyed Node
                [ ul
                    [ id "search-box-for-"
                    , class "p-0 m-0 text-mt-grey-dark"
                    ]
                    (List.indexedMap (displayItem selected autocompleteIndex) items)
                ]


displayItem : Maybe (Item item) -> Maybe Int -> Int -> Item item -> Html (Msg item)
displayItem selected autocompleteIndex index item =
    let
        isHighlighted =
            Just index == autocompleteIndex

        isSelected =
            Just item.id == (selected |> Maybe.map .id)

        titleText =
            item.label
    in
    li
        [ class "px-6 md:px-4 py-2 md:py-0"
        , class "text-lg md:text-base font-normal"
        , class "border border-transparent hover:bg-mt-blue-lightest cursor-pointer"
        , class "truncate"
        , id <| "elm-search-index-" ++ String.fromInt index
        , classList
            [ ( "text-mt-purple", isSelected )
            , ( "bg-mt-purple-lighter", isHighlighted )
            , ( "text-mt-grey-dark", item.isSelectable )
            , ( "text-mt-blue", not item.isSelectable )
            ]
        , value item.id
        , onClickStopPropagation (Set (Select item))
        , onEnter (Set (Select item))
        , attribute "data-role" "item-row"
        , title titleText
        ]
        [ span [ class "font-light" ] [ text titleText ]
        ]


searchInputView model =
    let
        records =
            model.search.items

        msgEvents =
            msgForKeyboardEvent records model.autocompleteIndex
    in
    div
        [ style "color" "red"
        ]
        [ Icons.Search.viewWithAttributes
            [ Svg.Attributes.height "1.5rem" ]
        , input
            [ type_ "text"
            , id <| domId model.label
            , onInput (Set << SearchTerms)
            , value (model.search.terms |> Maybe.withDefault "")
            , placeholder <| "Select or search"
            , autocomplete False -- turn off browser 'past history' autocomplete
            , Select.KeyShortcutEvents.on msgEvents
            ]
            []
        , Icons.CloseCross.viewWithAttributes
            [ Svg.Attributes.height "1.5rem"
            , onClick (Set (Mode Closed))
            ]
        ]


basicInit : msg -> String -> Int -> List (Item item) -> Maybe (Item item) -> (item -> Item item) -> (Search item -> ( Search item, Maybe (Msg item) )) -> Model item
basicInit searchMsg label searchDebounceInterval baseItems selected toItem runSearch =
    { label = label
    , selected = selected
    , toItem = toItem
    , runSearch = runSearch
    , search =
        { filters = []
        , terms = Nothing
        , items = baseItems
        , results = RemoteData.NotAsked
        }

    --  Internal Config
    , mode = Closed
    , searchDebouncer = Debouncer.toDebouncer (Debouncer.debounce searchDebounceInterval)
    , autocompleteIndex = Nothing
    , allowMultipleFilters = True
    , minimumSearchTermsLength = 0
    }



--  NOTE: Could be split out


msgForKeyboardEvent : Items a -> Maybe Int -> List ( String, Msg a )
msgForKeyboardEvent records autocompleteIndex =
    let
        { onFirst, onLast, noAutocompleteIndex } =
            autocompleteIndexDetails autocompleteIndex records

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
    [ ( "Escape", Set (Mode Closed) )
    , ( "Enter", Autocomplete )
    , ( "Tab", Autocomplete )
    , ( "ArrowDown", downOp )
    , ( "ArrowUp", upOp )
    ]


autocompleteIndexDetails : Maybe Int -> Items a -> AutocompletePositionalDetails
autocompleteIndexDetails currentIndex records =
    let
        autocompleteListLength =
            records |> List.length

        onLast =
            currentIndex == Just (autocompleteListLength - 1)

        onFirst =
            currentIndex == Just 0

        noAutocomplete =
            isNothing currentIndex
    in
    { onFirst = onFirst
    , onLast = onLast
    , noAutocompleteIndex = noAutocomplete
    }


type alias AutocompletePositionalDetails =
    { onFirst : Bool, onLast : Bool, noAutocompleteIndex : Bool }


sendMsg : Msg item -> Cmd (Msg item)
sendMsg msg =
    msg
        |> Task.succeed
        |> Task.perform identity


isNothing : Maybe a -> Bool
isNothing mebe =
    mebe == Nothing


domId : String -> String
domId label =
    "elm-select-search-" ++ label
