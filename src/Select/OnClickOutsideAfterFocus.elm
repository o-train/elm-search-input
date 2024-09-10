module Select.OnClickOutsideAfterFocus exposing (withId, succeedIfClickIsOustideOfId)

{-|

@docs withId, succeedIfClickIsOustideOfId

-}

-- From https://github.com/xarvh/elm-onclickoutside/blob/master/src/Html/OnClickOutside.elm
-- but didn't seem to be on elm packages site

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)


succeedIfBloodlineHasId : String -> Decoder ()
succeedIfBloodlineHasId targetId =
    Decode.field "id" Decode.string
        |> Decode.andThen
            (\id ->
                if id == targetId then
                    Decode.succeed ()

                else
                    Decode.field "parentNode" (succeedIfBloodlineHasId targetId)
            )


invertDecoder : Decoder a -> Decoder ()
invertDecoder decoder =
    let
        invertMaybe maybe =
            case maybe of
                Nothing ->
                    Decode.succeed ()

                Just _ ->
                    Decode.fail ""
    in
    Decode.maybe decoder
        |> Decode.andThen invertMaybe


{-| This is a Json.Decoder that you can use to decode a DOM
[FocusEvent](https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent).

It will _fail_ if `event.relatedTarget` or any of its ancestors have the
given DOM id.

It will succeed otherwise.

-}
succeedIfClickIsOustideOfId : String -> Decoder ()
succeedIfClickIsOustideOfId targetId =
    succeedIfBloodlineHasId targetId
        |> Decode.field "relatedTarget"
        |> invertDecoder


{-| The first argument is the DOM id that you want to assign to the element.

The second argument is the message that you want to trigger when the user
clicks outside the element.

The function returns a list of Html.Attributes to apply to the element.

The attributes are `tabindex`, `id` and the `focusout` event handler.

This function is meant to cover most use cases, but if you need more control
on the attributes, you will have to use
[succeedIfClickIsOustideOfId](#succeedIfClickIsOustideOfId) instead.

-}
withId : String -> msg -> List (Html.Attribute msg)
withId id onClickOutsideMsg =
    let
        decodeFocusOut =
            Decode.map (always onClickOutsideMsg) (succeedIfClickIsOustideOfId id)
    in
    [ Html.Attributes.id id
    , Html.Attributes.tabindex -1
    , Html.Events.on "focusout" decodeFocusOut
    ]
