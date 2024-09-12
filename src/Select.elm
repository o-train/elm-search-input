module Select exposing
    ( Item, Model, Msg(..), OpenState(..), Search, Setters(..)
    , basicInit
    , update, view, isOpen
    , SearchQuery(..), setSearchQuery
    )

{-| simple elm search/select input with debouncer

This is in Alpha and is currently not intended for external use (yet).

Currently working on stylings (elm-css or supply your own css)

See a full example of the select input [here](https://gitlab.com/o-train/elm-search-input/-/blob/main/examples/Example.elm)


# Types

@docs Item, Model, Msg, OpenState, Search, Setters


# Configuration

@docs basicInit


# Usage

@docs update, view, isOpen

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
import RemoteData exposing (RemoteData, WebData)
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


type Filter
    = Checkbox Bool
    | TextBox String


type alias Filters =
    List Filter


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


makeRequest : (item -> Item item) -> Request item -> Cmd (Msg item)
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


type alias CustomSearch item =
    Search item -> ( Search item, Cmd (Msg item) )



-- Move query into Query member below


{-| Select input's search query

  - Basic will search label fields on items
  - HttpQuery provide a req and decoder and it will update details
  - CustomQuery -> As it sounds

-}
type SearchQuery item
    = BasicQuery
    | HttpQuery (Maybe String -> Request item)
    | CustomQuery (CustomSearch item)


{-| Select input's search configuration
-}
type alias Search item =
    { filters : Filters
    , terms : Maybe String
    , items : List (Item item)
    , results : WebData (Items item)
    , query : SearchQuery item
    }


{-| Select Input configuration model
-}
type alias Model item =
    { label : String
    , selected : Maybe (Item item)

    -- Search/Mappers
    -- Should be a Msg?
    , toItem : item -> Item item

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


{-| Select Input messages
-}
type Msg item
    = Autocomplete
    | Clear
    | Set (Setters item)
    | RunSearch
    | QueueSearch (Debouncer.Msg (Msg item))
    | NoOp


{-| Select Input internal state setters
-}
type Setters item
    = Mode OpenState
    | Select (Item item)
    | Deselect (Item item)
    | AutocompleteIndex Direction
    | SearchTerms String
    | SearchResults (WebData (Items item))



-- | Filter Filter


{-| Update function in future it will have a third el to its tuple for synchronisation
-}
update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        Autocomplete ->
            ( model
                |> setAutocomplete
                |> setClosed
            , Cmd.none
            )

        Clear ->
            ( model |> clear
            , Cmd.none
            )

        Set (Mode Closed) ->
            ( model |> setClosed
            , Cmd.none
            )

        Set (Mode Opened) ->
            ( model |> setMode Opened
            , focusSearch model.label
            )

        Set (Select item) ->
            ( model
                |> setSelected item
                |> setClosed
            , Cmd.none
            )

        Set (Deselect item) ->
            ( { model | selected = Nothing }
            , Cmd.none
            )

        Set (AutocompleteIndex dir) ->
            ( model |> navigateAutocompleteIndex dir
            , Cmd.none
              -- Scroll to be added. Best may be to just ensure that current el is within viewport of other el otherwise + el height. Or port to JS?
              -- , autocompleteIndex |> Maybe.withDefault 9999 |> itemDomId |> scrollToEnd (domId model.label)
            )

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
            ( model.search
                |> setSearchTerms (Just terms)
                |> setSearchResults RemoteData.NotAsked
                |> flip setSearch model
                |> setAutocompleteIndex autocompleteIndex
            , cmd
            )

        Set (SearchResults results) ->
            ( results
                |> RemoteData.map (List.filter <| not << isSelected model.selected)
                |> flip setSearchResults model.search
                |> flip setSearch model
            , Cmd.none
            )

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
                        ( m, c ) =
                            update emitted updatedModel
                    in
                    ( m, Cmd.batch [ c, mappedCmd ] )

                Nothing ->
                    ( updatedModel, mappedCmd )

        RunSearch ->
            searchItems model

        NoOp ->
            ( model, Cmd.none )


{-| Display search input and results
-}
view : Model a -> Html (Msg a)
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


itemsView : Int -> Maybe (Item item) -> WebData (Items item) -> Maybe Int -> Html (Msg item)
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


itemView : Maybe (Item item) -> Maybe Int -> Int -> Item item -> Html (Msg item)
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


selectedView : Maybe (Item item) -> Html (Msg item)
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


searchInputView : Model item -> Html (Msg item)
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
        [ Select.Icons.Search.viewWithAttributes
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
        , Select.Icons.CloseCross.viewWithAttributes
            [ Svg.Attributes.class "elm-icon"
            , onClick Clear
            ]
        ]


{-| Minimum model creation. This will be split out into 'RequiredModel' in future
-}
basicInit : String -> Int -> List (Item item) -> Maybe (Item item) -> (item -> Item item) -> SearchQuery item -> Model item
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


msgForKeyboardEvent : Items a -> Maybe Int -> List ( String, Msg a )
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
isOpen : Model item -> Bool
isOpen model =
    model.mode == Opened


sendMsg : Msg item -> Cmd (Msg item)
sendMsg msg =
    msg
        |> Task.succeed
        |> Task.perform identity


isNothing : Maybe a -> Bool
isNothing mebe =
    mebe == Nothing


focusSearch : String -> Cmd (Msg item)
focusSearch label =
    Browser.Dom.focus (domInputId label) |> Task.attempt (always NoOp)


domId : String -> String
domId label =
    "elm-search-" ++ label


domInputId : String -> String
domInputId label =
    "elm-select-search-" ++ label


searchItems : Model item -> ( Model item, Cmd (Msg item) )
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
    ( { model | search = search }, cmd )


basicSearch : Maybe (Item item) -> Search item -> Search item
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


scrollToEnd : String -> String -> Cmd (Msg item)
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



-- Setters


setAutocomplete : Model item -> Model item
setAutocomplete model =
    model.autocompleteIndex
        |> Maybe.andThen (flip List.Extra.getAt (RemoteData.withDefault [] model.search.results))
        |> Maybe.map (flip setSelected model)
        |> Maybe.withDefault model


setAutocompleteIndex : Maybe Int -> Model item -> Model item
setAutocompleteIndex autocompleteIndex model =
    { model | autocompleteIndex = autocompleteIndex }


navigateAutocompleteIndex : Direction -> Model item -> Model item
navigateAutocompleteIndex dir model =
    model.autocompleteIndex
        |> Select.AutocompleteLogic.changeStep dir
        |> flip setAutocompleteIndex model


setSelected : Item item -> Model item -> Model item
setSelected item model =
    { model | selected = Just item }


setClosed : Model item -> Model item
setClosed model =
    model
        |> clear
        |> setMode Closed


clear : Model item -> Model item
clear model =
    model.search
        |> setSearchTerms Nothing
        |> setSearchResults RemoteData.NotAsked
        |> flip setSearch model


setSearch : Search item -> Model item -> Model item
setSearch search model =
    { model | search = search }



-- Not used atm


setSearchQuery : SearchQuery item -> Model item -> Model item
setSearchQuery query ({ search } as model) =
    { search | query = query }
        |> flip setSearch model


setSearchTerms : Maybe String -> Search item -> Search item
setSearchTerms terms search =
    { search | terms = terms }


setSearchResults : WebData (Items item) -> Search item -> Search item
setSearchResults results search =
    { search | results = results }


setMode : OpenState -> Model item -> Model item
setMode mode model =
    { model | mode = mode }


flip : (a -> b -> c) -> b -> a -> c
flip fn a b =
    fn b a
