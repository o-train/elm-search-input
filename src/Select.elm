module Select exposing
    ( Item, Model, Msg(..), OpenState(..), Search, Setters(..), Filter(..), ParentMsg(..), SearchQuery(..)
    , basicInit
    , update, view, isOpen, setSearchQuery, addFilter
    )

{-| simple elm search/select input with debouncer

This is in Alpha and is currently not intended for external use (yet).

Currently working on stylings (elm-css or supply your own css)

See a full example of the select input [here](https://gitlab.com/o-train/elm-search-input/-/blob/main/examples/Example.elm)


# Types

@docs Item, Model, Msg, OpenState, Search, Setters, Filter, ParentMsg, SearchQuery


# Configuration

@docs basicInit


# Usage

@docs update, view, isOpen, setSearchQuery, addFilter

-}

import Browser.Dom
import Debouncer.Basic as Debouncer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onClickStopPropagation, onEnter)
import Html.Extra
import Http exposing (Header)
import Json.Decode exposing (Decoder)
import List.Extra
import RemoteData exposing (WebData)
import Select.Autocomplete exposing (Direction(..))
import Select.AutocompleteLogic
import Select.Icons.CloseCross
import Select.Icons.Search
import Select.KeyShortcutEvents
import Select.OnClickOutsideAfterFocus
import Svg.Attributes
import Task


{-| Dropdown open state
-}
type OpenState
    = Opened
    | Closed


{-| Filter - bring your own with parent msg. Others to come.
-}
type Filter item msg
    = CustomFilter (Html (Msg item msg))



-- | Checkbox Bool
-- | TextBox String


type alias Filters item msg =
    List (Filter item msg)


{-| An item for selection
-}
type alias Item item =
    { id : String
    , label : String
    , identity : item
    , isSelectable : Bool
    }


type alias Items item =
    List (Item item)


type alias Request item =
    { url : String
    , headers : List Header
    , method : String
    , body : Http.Body
    , decoder : Decoder (List item)
    }


makeRequest : (item -> Item item) -> Request item -> Cmd (Msg item msg)
makeRequest toItemFn req =
    Http.request
        { url = req.url
        , method = req.method
        , headers = req.headers
        , body = req.body
        , timeout = Nothing
        , tracker = Nothing
        , expect =
            Http.expectJson (Set << SearchResults << RemoteData.fromResult << Result.map (List.map toItemFn)) req.decoder
        }


type alias CustomSearch item msg =
    Search item msg -> ( Search item msg, Cmd (Msg item msg) )



-- Move query into Query member below


{-| Select input's search query

  - Basic will search label fields on items
  - HttpQuery provide a req and decoder and it will update details
  - CustomQuery -> As it sounds

-}
type SearchQuery item msg
    = BasicQuery
    | HttpQuery (Maybe String -> Request item)
    | CustomQuery (CustomSearch item msg)


{-| Select input's search configuration
-}
type alias Search item msg =
    { filters : Filters item msg
    , terms : Maybe String
    , items : List (Item item)
    , results : WebData (Items item)
    , query : SearchQuery item msg
    }


{-| Select Input configuration model
-}
type alias Model item msg =
    { label : String
    , selected : Maybe (Item item)

    -- Search/Mappers
    -- Should be a Msg?
    , toItem : item -> Item item

    --  Config
    , search : Search item msg

    --  Internal Config
    , mode : OpenState
    , searchDebouncer : Debouncer.Debouncer (Msg item msg) (Msg item msg)
    , autocompleteIndex : Maybe Int

    --  Advanced Config
    , allowMultipleFilters : Bool
    , minimumSearchTermsLength : Int
    }


{-| Select Input messages
-}
type Msg item msg
    = Autocomplete
    | Clear
    | Set (Setters item msg)
    | RunSearch
    | EmitMsg msg
    | QueueSearch (Debouncer.Msg (Msg item msg))
    | NoOp


{-| Select Input internal state setters
-}
type Setters item msg
    = Mode OpenState
    | Select (Item item)
    | Deselect (Item item)
    | AutocompleteIndex Direction
    | SearchTerms String
    | SearchResults (WebData (Items item))
    | SelectFilter (Filter item msg)


{-| Return msgs for your application to respond to
-}
type ParentMsg item msg
    = SelectedItem (Item item)
    | ParentMsg msg
    | NoChange



-- | Filter Filter


type alias Return item msg =
    ( Model item msg, ParentMsg item msg, Cmd (Msg item msg) )


{-| Update function in future it will have a third el to its tuple for synchronisation
-}
update : Msg item msg -> Model item msg -> Return item msg
update msg model =
    case msg of
        Autocomplete ->
            model
                |> setAutocomplete
                |> setClosed
                |> returnSingleton

        Clear ->
            model
                |> clear
                |> returnSingleton

        Set (Mode Closed) ->
            model
                |> setClosed
                |> returnSingleton

        Set (Mode Opened) ->
            model
                |> setMode Opened
                |> returnSingleton
                |> returnCmd (focusSearch model.label)

        Set (Select item) ->
            model
                |> setSelected item
                |> setClosed
                |> returnSingleton
                |> returnParentMsg (SelectedItem item)

        Set (Deselect item) ->
            { model | selected = Nothing }
                |> returnSingleton

        Set (AutocompleteIndex dir) ->
            model
                |> navigateAutocompleteIndex dir
                |> returnSingleton

        -- Scroll to be added. Best may be to just ensure that current el is within viewport of other el otherwise + el height. Or port to JS?
        -- , autocompleteIndex |> Maybe.withDefault 9999 |> itemDomId |> scrollToEnd (domId model.label)
        Set (SearchTerms terms) ->
            let
                sanitisedTerms =
                    String.trim terms

                runnable =
                    String.length sanitisedTerms >= model.minimumSearchTermsLength

                cmd =
                    if runnable then
                        RunSearch
                            |> Debouncer.provideInput
                            |> QueueSearch
                            |> sendMsg

                    else
                        Cmd.none

                autocompleteIndex =
                    if String.length sanitisedTerms > 0 then
                        Just 0

                    else
                        Nothing
            in
            model.search
                |> setSearchTerms (Just terms)
                |> setSearchResults RemoteData.NotAsked
                |> flip setSearch model
                |> setAutocompleteIndex autocompleteIndex
                |> returnSingleton
                |> returnCmd cmd

        Set (SearchResults results) ->
            results
                |> RemoteData.map (List.filter <| not << isSelected model.selected)
                |> flip setSearchResults model.search
                |> flip setSearch model
                |> returnSingleton

        Set (SelectFilter filter) ->
            model
                |> returnSingleton

        --  case filter  of
        --      CustomFilter parentMsg ->
        --        (model, Cmd.none)
        EmitMsg parentMsg ->
            ( model, ParentMsg parentMsg, Cmd.none )

        QueueSearch subMsg ->
            let
                ( debouncedModel, subCmd, emittedMsg ) =
                    Debouncer.update subMsg model.searchDebouncer

                mappedCmd =
                    Cmd.map QueueSearch subCmd

                updatedModel =
                    { model | searchDebouncer = debouncedModel }
            in
            case emittedMsg of
                Just emitted ->
                    let
                        ( m, parentMsg, c ) =
                            update emitted updatedModel
                    in
                    ( m, parentMsg, Cmd.batch [ c, mappedCmd ] )

                Nothing ->
                    ( updatedModel, NoChange, mappedCmd )

        RunSearch ->
            model
                |> searchItems

        NoOp ->
            model
                |> returnSingleton


{-| Display search input and results
-}
view : Model item msg -> Html (Msg item msg)
view model =
    let
        items =
            model.search.terms
                |> Maybe.map
                    (\t ->
                        if String.trim t /= "" then
                            model.search.results

                        else
                            RemoteData.succeed model.search.items
                    )
                |> Maybe.withDefault (RemoteData.succeed model.search.items)
    in
    div
        (class "elm-search-input-container"
            :: Select.OnClickOutsideAfterFocus.withId (domId model.label) (Set (Mode Closed))
        )
        [ searchInputView model
        , selectedView model.selected
        , itemsView model.minimumSearchTermsLength model.selected items model.autocompleteIndex
        ]


itemsView : Int -> Maybe (Item item) -> WebData (Items item) -> Maybe Int -> Html (Msg item msg)
itemsView min selected webdataItems autocompleteIndex =
    case webdataItems of
        RemoteData.NotAsked ->
            span [ class "px-4 text-grey" ] [ text <| "Waiting for at least " ++ String.fromInt min ++ " letters..." ]

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
                    [ id "select-item-for-"
                    , class "p-0 m-0 text-mt-grey-dark"
                    ]
                    (List.indexedMap (itemView selected autocompleteIndex) items)
                ]


itemView : Maybe (Item item) -> Maybe Int -> Int -> Item item -> Html (Msg item msg)
itemView selected autocompleteIndex index item =
    let
        isHighlighted =
            Just index == autocompleteIndex

        titleText =
            item.label
    in
    li
        [ class "elm-search-item"
        , id <| itemDomId index
        , classList
            [ ( "text-mt-purple", isSelected selected item )
            , ( "bg-mt-purple-lighter", isHighlighted )
            , ( "text-mt-grey-dark", item.isSelectable )
            , ( "elm-disabled", not item.isSelectable )
            ]
        , value item.id
        , onClickStopPropagation (Set (Select item))
        , onEnter (Set (Select item))
        , attribute "data-role" "item-row"
        , title titleText
        ]
        [ span [ class "font-light" ] [ text titleText ]
        ]


selectedView : Maybe (Item item) -> Html (Msg item msg)
selectedView selected =
    selected
        |> Maybe.map
            (\item ->
                div
                    [ class "elm-search-item"
                    , class "elm-selected"
                    , id <| "elm-search-selected"
                    , onClickStopPropagation (Set (Deselect item))
                    , attribute "data-role" "item-row"
                    , title item.label
                    ]
                    [ span [ class "font-light" ] [ text item.label ]
                    ]
            )
        |> Maybe.withDefault Html.Extra.nothing


searchInputView : Model item msg -> Html (Msg item msg)
searchInputView model =
    let
        records =
            model.search.items

        msgEvents =
            msgForKeyboardEvent records model.autocompleteIndex
    in
    div
        [ class "elm-search-input"
        ]
        ([ Select.Icons.Search.viewWithAttributes
            [ Svg.Attributes.class "elm-icon" ]
         , input
            [ class "elm-search-text-input"
            , type_ "text"
            , id <| domInputId model.label
            , onInput (Set << SearchTerms)
            , value (model.search.terms |> Maybe.withDefault "")
            , placeholder <| "Select or search"
            , autocomplete False -- turn off browser 'past history' autocomplete
            , Select.KeyShortcutEvents.on msgEvents
            ]
            []
         ]
            ++ filtersView model.search
        )


filtersView : Search item msg -> List (Html (Msg item msg))
filtersView search =
    let
        ( closeTitle, closeMsg ) =
            if search.terms == Nothing || search.terms == Just "" then
                ( "Close Search", Set (Mode Closed) )

            else
                ( "Clear", Clear )
    in
    search.filters
        |> List.map filterView
        |> flip List.append
            [ Select.Icons.CloseCross.viewWithAttributes
                [ Svg.Attributes.class "elm-icon"
                , title closeTitle
                , onClick closeMsg
                ]
            ]


filterView : Filter item msg -> Html (Msg item msg)
filterView filter =
    case filter of
        CustomFilter v ->
            v


{-| Minimum model creation. This will be split out into 'RequiredModel' in future
-}
basicInit : String -> Int -> List (Item item) -> Maybe (Item item) -> (item -> Item item) -> SearchQuery item msg -> Model item msg
basicInit label searchDebounceInterval baseItems selected toItem searchQuery =
    { label = label
    , selected = selected
    , toItem = toItem
    , search =
        { filters = []
        , terms = Nothing
        , items = baseItems
        , results = RemoteData.NotAsked
        , query = searchQuery
        }

    --  Internal Config
    , mode = Closed
    , searchDebouncer = Debouncer.toDebouncer (Debouncer.debounce searchDebounceInterval)
    , autocompleteIndex = Nothing
    , allowMultipleFilters = True
    , minimumSearchTermsLength = 0
    }



--  NOTE: Could be split out


msgForKeyboardEvent : Items item -> Maybe Int -> List ( String, Msg item msg )
msgForKeyboardEvent records autocompleteIndex =
    let
        { onFirst, onLast, noAutocompleteIndex } =
            autocompleteIndexDetails autocompleteIndex records

        downOp =
            if not onLast || noAutocompleteIndex then
                Set (AutocompleteIndex Next)

            else
                NoOp

        upOp =
            if not onFirst then
                Set (AutocompleteIndex Previous)

            else
                NoOp
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


{-| Check if selector is open or not
-}
isOpen : Model item msg -> Bool
isOpen model =
    model.mode == Opened


sendMsg : Msg item msg -> Cmd (Msg item msg)
sendMsg msg =
    msg
        |> Task.succeed
        |> Task.perform identity


isNothing : Maybe a -> Bool
isNothing mebe =
    mebe == Nothing


focusSearch : String -> Cmd (Msg item msg)
focusSearch label =
    Browser.Dom.focus (domInputId label) |> Task.attempt (always NoOp)


domId : String -> String
domId label =
    "elm-search-" ++ label


domInputId : String -> String
domInputId label =
    "elm-select-search-" ++ label


searchItems : Model item msg -> Return item msg
searchItems model =
    let
        ( search, cmd ) =
            case model.search.query of
                BasicQuery ->
                    ( basicSearch model.selected model.search, Cmd.none )

                HttpQuery req ->
                    ( model.search, makeRequest model.toItem (req model.search.terms) )

                CustomQuery customSearch ->
                    customSearch model.search
    in
    ( { model | search = search }, NoChange, cmd )


basicSearch : Maybe (Item item) -> Search item msg -> Search item msg
basicSearch selected model =
    let
        anyMatch description term =
            description
                |> String.toLower
                |> String.split " "
                |> List.any (String.startsWith (String.toLower term))

        matchesSearch filterString item =
            if isSelected selected item then
                False

            else
                let
                    stringForFiltering =
                        [ item.label ]
                in
                filterString
                    |> String.split " "
                    |> List.all (anyMatch (String.join " " stringForFiltering))

        items =
            case model.terms of
                Just filterString ->
                    List.filter (matchesSearch filterString) model.items

                Nothing ->
                    model.items
    in
    { model | results = RemoteData.Success items }


isSelected : Maybe (Item item) -> Item item -> Bool
isSelected selected item =
    Just item.id == (selected |> Maybe.map .id)


scrollToEnd : String -> String -> Cmd (Msg item msg)
scrollToEnd parentId id =
    Task.map3
        (\outerVp outerE innerE ->
            let
                _ =
                    Debug.log "OuterVp" outerVp

                _ =
                    Debug.log "OuterE" outerE

                _ =
                    Debug.log "InnerE" innerE
            in
            outerVp.viewport.y
                + innerE.element.y
                + 0
                * innerE.element.height
                - outerE.element.y
                - 0
                * outerVp.viewport.height
                |> Browser.Dom.setViewportOf parentId outerVp.viewport.x
        )
        (Browser.Dom.getViewportOf parentId)
        (Browser.Dom.getElement parentId)
        (Browser.Dom.getElement id)
        |> Task.attempt (\_ -> NoOp)


itemDomId : Int -> String
itemDomId index =
    "elm-search-index-" ++ String.fromInt index


returnSingleton : Model item msg -> Return item msg
returnSingleton model =
    ( model, NoChange, Cmd.none )


returnCmd : Cmd (Msg item msg) -> Return item msg -> Return item msg
returnCmd cmd return =
    case return of
        ( m, p, c ) ->
            ( m, p, Cmd.batch [ c, cmd ] )


returnParentMsg : ParentMsg item msg -> Return item msg -> Return item msg
returnParentMsg parentMsg return =
    case return of
        ( m, _, c ) ->
            ( m, parentMsg, c )



-- Internal and External Setters


setAutocomplete : Model item msg -> Model item msg
setAutocomplete model =
    model.autocompleteIndex
        |> Maybe.andThen (flip List.Extra.getAt (RemoteData.withDefault [] model.search.results))
        |> Maybe.map (flip setSelected model)
        |> Maybe.withDefault model


setAutocompleteIndex : Maybe Int -> Model item msg -> Model item msg
setAutocompleteIndex autocompleteIndex model =
    { model | autocompleteIndex = autocompleteIndex }


navigateAutocompleteIndex : Direction -> Model item msg -> Model item msg
navigateAutocompleteIndex dir model =
    model.autocompleteIndex
        |> Select.AutocompleteLogic.changeStep dir
        |> flip setAutocompleteIndex model


setSelected : Item item -> Model item msg -> Model item msg
setSelected item model =
    { model | selected = Just item }


setClosed : Model item msg -> Model item msg
setClosed model =
    model
        |> clear
        |> setMode Closed


clear : Model item msg -> Model item msg
clear model =
    model.search
        |> setSearchTerms Nothing
        |> setSearchResults RemoteData.NotAsked
        |> flip setSearch model


setSearch : Search item msg -> Model item msg -> Model item msg
setSearch search model =
    { model | search = search }


{-| Add a SearchQuery for usage when search input changes
-}
setSearchQuery : SearchQuery item msg -> Model item msg -> Model item msg
setSearchQuery query ({ search } as model) =
    { search | query = query }
        |> flip setSearch model


setSearchTerms : Maybe String -> Search item msg -> Search item msg
setSearchTerms terms search =
    { search | terms = terms }


setSearchResults : WebData (Items item) -> Search item msg -> Search item msg
setSearchResults results search =
    { search | results = results }


setFilters : Filters item msg -> Search item msg -> Search item msg
setFilters filters search =
    { search | filters = filters }


setMode : OpenState -> Model item msg -> Model item msg
setMode mode model =
    { model | mode = mode }


flip : (a -> b -> c) -> b -> a -> c
flip fn a b =
    fn b a



-- Init Config Setters


{-| Initialise select input with a filter
example
basicInit yourArgs
|> addFilter (Select.CustomFilter yourView)
-}
addFilter : Filter item msg -> Model item msg -> Model item msg
addFilter filter model =
    filter
        :: model.search.filters
        |> flip setFilters model.search
        |> flip setSearch model
