port module BrowserAction exposing (..)

import Background exposing (Msg(..))
import Browser
import Common as C
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Url.Parser exposing ((<?>))



-- Primary types


type alias Model =
    { common : CommonModel
    }


type Msg
    = ReceiveMessage Json.Encode.Value



-- Helper types


type CommonModel
    = Valid C.Model
    | Invalid String
    | Waiting



-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg



-- Primary functions


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> subs
        }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( { common = Waiting }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        ReceiveMessage value ->
            case Json.Decode.decodeValue C.messageFromBackgroundScriptDecoder value of
                Ok message ->
                    case message of
                        C.SendModel commonModel ->
                            ( { model | common = Valid commonModel }
                            , Cmd.none
                            )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text "Hello world"


subs : Sub Msg
subs =
    Sub.batch
        [ receiveMessage ReceiveMessage ]



-- Helper functions


decodeCommonModel : Json.Decode.Value -> CommonModel
decodeCommonModel flags =
    case Json.Decode.decodeValue C.modelDecoder flags of
        Ok commonModel ->
            Valid commonModel

        Err e ->
            Invalid (Json.Decode.errorToString e)
