port module Background exposing (..)

import Common as C
import Json.Decode
import Json.Encode
import Url


main : Program Json.Decode.Value C.Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subs
        }


type Msg
    = GotUrlChange Json.Encode.Value
    | GotModelRequest Json.Encode.Value


checkIfRedirect : List String -> Url.Url -> Bool
checkIfRedirect domains url =
    let
        hostname =
            url.host

        doesMatch : String -> Bool
        doesMatch domain =
            String.endsWith domain hostname
    in
    domains
        |> List.any doesMatch


update : Msg -> C.Model -> ( C.Model, Cmd msg )
update msg model =
    let
        ( newModel, cmd ) =
            innerUpdate msg model
    in
    ( newModel, Cmd.batch [ cmd, sendModel <| C.encodeModel newModel ] )


innerUpdate : Msg -> C.Model -> ( C.Model, Cmd msg )
innerUpdate msg model =
    case msg of
        GotModelRequest _ ->
            ( model, sendModel <| C.encodeModel model )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok ( tabId, urlStr ) ->
                    case Url.fromString urlStr of
                        Just url ->
                            if checkIfRedirect model.domainsToRedirect url then
                                ( model
                                , setRedirect
                                    (encodeRedirect tabId
                                        "/intercept.html"
                                    )
                                )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            -- TODO complain?
                            ( model, Cmd.none )

                Err _ ->
                    -- TODO complain?
                    ( model, Cmd.none )


init flags =
    ( { domainsToRedirect =
            [ "reddit.com"
            ]
      }
    , Cmd.none
    )


subs _ =
    Sub.batch
        [ getUrlChange GotUrlChange
        , receiveModelRequest GotModelRequest
        ]


port receiveModelRequest : (Json.Encode.Value -> msg) -> Sub msg


port getUrlChange : (Json.Encode.Value -> msg) -> Sub msg


port setRedirect : Json.Encode.Value -> Cmd msg


port sendModel : Json.Encode.Value -> Cmd msg


decodeUrlChange : Json.Decode.Value -> Result Json.Decode.Error ( Int, String )
decodeUrlChange urlChangeJson =
    let
        urlChangeDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "tabId" Json.Decode.int)
                (Json.Decode.field "url" Json.Decode.string)
    in
    Json.Decode.decodeValue urlChangeDecoder urlChangeJson


encodeRedirect : Int -> String -> Json.Encode.Value
encodeRedirect tabId url =
    Json.Encode.object
        [ ( "tabId", Json.Encode.int tabId )
        , ( "url", Json.Encode.string url )
        ]
