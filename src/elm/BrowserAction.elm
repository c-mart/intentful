port module BrowserAction exposing (..)

import Background exposing (Msg(..))
import Browser
import Common as C
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Url
import Url.Parser exposing ((<?>))



-- Primary types


type AppValidity
    = AppValid Model
    | AppInvalid String


type alias Model =
    { common : C.Model
    , currentTabUrl : Url.Url
    }


type Msg
    = ReceiveMessage Json.Encode.Value



-- Helper types
-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg



-- Primary functions


main : Program Json.Encode.Value AppValidity Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> subs
        }


init : Json.Decode.Value -> ( AppValidity, Cmd Msg )
init flags =
    let
        commonModel =
            Json.Decode.decodeValue (Json.Decode.field "common-model" C.modelDecoder) flags

        currentTabUrl =
            Json.Decode.decodeValue
                (Json.Decode.field "currentTabUrl" Json.Decode.string
                    |> Json.Decode.map Url.fromString
                )
                flags
    in
    case ( commonModel, currentTabUrl ) of
        ( Ok m, Ok (Just u) ) ->
            ( AppValid <| Model m u, Cmd.none )

        ( Err decodeErr, _ ) ->
            ( AppInvalid (Json.Decode.errorToString decodeErr), Cmd.none )

        ( _, Err decodeErr ) ->
            ( AppInvalid (Json.Decode.errorToString decodeErr), Cmd.none )

        ( _, Ok Nothing ) ->
            ( AppInvalid "could not parse current URL", Cmd.none )


update : Msg -> AppValidity -> ( AppValidity, Cmd a )
update msg validity =
    case validity of
        AppValid model ->
            updateValid msg model
                |> Tuple.mapFirst AppValid

        AppInvalid _ ->
            ( validity, Cmd.none )


updateValid : Msg -> Model -> ( Model, Cmd a )
updateValid msg model =
    case msg of
        ReceiveMessage value ->
            case Json.Decode.decodeValue C.messageFromBackgroundScriptDecoder value of
                Ok message ->
                    case message of
                        C.SendModel commonModel ->
                            ( { model | common = commonModel }
                            , Cmd.none
                            )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )


view : AppValidity -> Html Msg
view validity =
    let
        showErr errStr =
            Html.text ("Cannot render page: " ++ errStr)
    in
    case validity of
        AppValid model ->
            viewValid model

        AppInvalid e ->
            showErr e


viewValid : Model -> Html Msg
viewValid model =
    Html.text ("Current URL is " ++ Url.toString model.currentTabUrl)


subs : Sub Msg
subs =
    Sub.batch
        [ receiveMessage ReceiveMessage ]



-- Helper functions
