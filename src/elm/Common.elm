module Common exposing (..)

import Json.Decode
import Json.Encode
import Time
import Url



-- Types


type alias Model =
    { unsafeDomains : List String
    , exceptions : List Exception
    }


type alias Exception =
    { domain : String
    , endTime : Time.Posix
    }


type MessageFromBackgroundScript
    = SendModel Model


type MessageFromInterceptPage
    = RequestModel
    | NewException Exception



-- Encoders


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "unsafeDomains"
          , Json.Encode.list Json.Encode.string model.unsafeDomains
          )
        , ( "exceptions", Json.Encode.list encodeException model.exceptions )
        ]


encodeException : Exception -> Json.Encode.Value
encodeException exception =
    Json.Encode.object
        [ ( "domain", Json.Encode.string exception.domain )
        , ( "endTime", Json.Encode.int <| Time.posixToMillis exception.endTime )
        ]


encodeMessageFromBackgroundScript : MessageFromBackgroundScript -> Json.Encode.Value
encodeMessageFromBackgroundScript message =
    case message of
        SendModel model ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "send-model" )
                , ( "model", encodeModel model )
                ]


encodeMessageFromInterceptPage : MessageFromInterceptPage -> Json.Encode.Value
encodeMessageFromInterceptPage message =
    case message of
        RequestModel ->
            Json.Encode.object [ ( "tag", Json.Encode.string "request-model" ) ]

        NewException exception ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "new-exception" )
                , ( "exception", encodeException exception )
                ]



-- Decoders


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.map2 Model
        (Json.Decode.field "unsafeDomains"
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.field "exceptions" (Json.Decode.list exceptionDecoder))


exceptionDecoder : Json.Decode.Decoder Exception
exceptionDecoder =
    Json.Decode.map2 Exception
        (Json.Decode.field "domain" Json.Decode.string)
        (Json.Decode.field "endTime" Json.Decode.int |> Json.Decode.map Time.millisToPosix)


messageFromInterceptPageDecoder : Json.Decode.Decoder MessageFromInterceptPage
messageFromInterceptPageDecoder =
    let
        decode tag =
            case tag of
                "request-model" ->
                    Json.Decode.succeed RequestModel

                "new-exception" ->
                    Json.Decode.map NewException <|
                        Json.Decode.field "exception" exceptionDecoder

                _ ->
                    Json.Decode.fail "Unrecognized message tag"
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen decode


messageFromBackgroundScriptDecoder : Json.Decode.Decoder MessageFromBackgroundScript
messageFromBackgroundScriptDecoder =
    let
        decode tag =
            case tag of
                "send-model" ->
                    Json.Decode.field "model" modelDecoder
                        |> Json.Decode.map SendModel

                _ ->
                    Json.Decode.fail "Unrecognized message tag"
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen decode



-- Helper functions


checkIfIntercept : Model -> Url.Url -> Bool
checkIfIntercept model url =
    let
        hostname =
            url.host

        doesMatch : String -> Bool
        doesMatch domain =
            String.endsWith domain hostname

        onRedirectList =
            model.unsafeDomains
                |> List.any doesMatch

        onExceptionList =
            List.map .domain model.exceptions
                |> List.any doesMatch
    in
    onRedirectList && not onExceptionList
