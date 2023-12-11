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
    | GotMessage Json.Encode.Value


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
    ( newModel, Cmd.batch [ cmd, sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel newModel) ] )


innerUpdate : Msg -> C.Model -> ( C.Model, Cmd msg )
innerUpdate msg model =
    case msg of
        GotMessage value ->
            case Json.Decode.decodeValue C.decodeMessageFromInterceptPage value of
                Ok message ->
                    case message of
                        C.RequestModel ->
                            ( model, sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel model) )

                        C.NewException exception ->
                            -- TODO store exception in model
                            ( model, Cmd.none )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok ( tabId, urlStr ) ->
                    case Url.fromString urlStr of
                        Just url ->
                            if checkIfRedirect model.domainsToRedirect url then
                                ( model
                                , setRedirect
                                    (encodeRedirect tabId <|
                                        String.concat
                                            [ "/intercept.html?next="
                                            , url |> Url.toString |> Url.percentEncode
                                            ]
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
        , receiveMessage GotMessage
        ]


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port getUrlChange : (Json.Encode.Value -> msg) -> Sub msg


port setRedirect : Json.Encode.Value -> Cmd msg


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
