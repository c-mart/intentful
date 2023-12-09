port module Intercept exposing (..)

import Browser
import Common as C
import Debug
import Html exposing (Html)
import Json.Decode
import Json.Encode


main : Program Json.Encode.Value OuterModel Msg
main =
    Browser.element
        { init = \_ -> ( WaitingForModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> subs
        }


type OuterModel
    = Valid C.Model
    | Invalid String
    | WaitingForModel


decodeModel : Json.Decode.Value -> OuterModel
decodeModel flags =
    case Json.Decode.decodeValue C.modelDecoder flags of
        Ok model ->
            Valid model

        Err e ->
            Invalid (Json.Decode.errorToString e)


type Msg
    = GotModel Json.Encode.Value


update : Msg -> OuterModel -> ( OuterModel, Cmd a )
update msg outerModel =
    case msg of
        GotModel value ->
            ( decodeModel value, Cmd.none )


view : OuterModel -> Html Msg
view outerModel =
    let
        text =
            case outerModel of
                WaitingForModel ->
                    "Waiting for model"

                Valid m ->
                    "Got model: " ++ Debug.toString m

                Invalid s ->
                    "Invalid model: " ++ s
    in
    Html.text text


subs : Sub msg
subs =
    receiveModel GotModel


port receiveModel : (Json.Encode.Value -> msg) -> Sub msg
