module Example exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Extra
import RemoteData
import Select exposing (Model, Msg(..), basicInit)


type alias User =
    { id : Int
    , name : String
    , age : Int
    }


type alias Model =
    { counter : Int
    , users : List User
    , currentUser : User
    , selectInput : Select.Model User
    }



-- fetchMatchingMatters : EditMode -> String -> Cmd Msg
-- fetchMatchingMatters editMode term =
--     Http.get
--         { url =
--             toUrl "api/matters/search"
--                 [ Param "term" <| Just (Url.percentEncode term)
--                 , Param "mode" <| Just <| EditMode.toString editMode
--                 ]
--         , expect = Http.expectJson (RemoteData.fromResult >> SearchedMattersFetched) decodeMatters
--         }


runSearch : Select.Search User -> ( Select.Search User, Maybe (Select.Msg User) )
runSearch model =
    let
        anyMatch description term =
            description
                |> String.toLower
                |> String.split " "
                |> List.any (String.startsWith (String.toLower term))

        matchesSearch filterString item =
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
    ( { model | results = RemoteData.Success items }, Nothing )


toItem : User -> Select.Item User
toItem user =
    { id = user.id |> String.fromInt
    , label = user.name
    , identity = user
    , isSelectable = True
    }


init : ( Model, Cmd Msg )
init =
    let
        items =
            List.map toItem fakeUsers
    in
    ( { counter = 0
      , users = fakeUsers
      , currentUser = luffy
      , selectInput = basicInit SetUser "Users" 0 items Nothing toItem runSearch
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


main : Program () Model Msg
main =
    Browser.document { init = always init, update = update, view = page, subscriptions = subscriptions }


page : Model -> Browser.Document Msg
page model =
    { title = "Elm Search Select"
    , body = [ view model ]
    }


type Msg
    = SetUser (Select.Msg User)
    | SelectInput (Select.Msg User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUser (Select.Set (Select.Select item)) ->
            ( { model | currentUser = item.identity }, Cmd.none )

        SelectInput subMsg ->
            let
                ( selectInputModel, selectMsg ) =
                    Select.update subMsg model.selectInput
            in
            ( { model | selectInput = selectInputModel }, selectMsg |> Cmd.map SelectInput )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (SelectInput (Select.Set (Select.Mode Select.Opened))) ] [ text "Search Users" ]
        , Html.Extra.viewIf (Select.isOpen model.selectInput) (Select.view model.selectInput |> Html.map SelectInput)
        ]


fakeUsers : List User
fakeUsers =
    [ luffy, zoro, nami ]


luffy : User
luffy =
    { id = 1, name = "Luffy", age = 14 }


zoro : User
zoro =
    { id = 2, name = "Zoro", age = 15 }


nami : User
nami =
    { id = 3, name = "Nami", age = 15 }
