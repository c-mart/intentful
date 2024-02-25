module Common exposing (..)

import Json.Decode
import Json.Encode
import Time
import Url



-- Types


type alias Model =
    -- TODO these should be sets, not lists
    { unsafeDomains : List String
    , safeDomains : List String
    , exceptions : List Exception
    }


type alias Exception =
    { domain : String
    , endTime : Time.Posix
    }


type DomainStatus
    = Unknown
    | Safe
    | Unsafe


type MessageFromBackgroundScript
    = SendModel Model


type MessageToBackgroundScript
    = RequestModel
    | NewException Exception
    | SetDomainStatus String DomainStatus



-- Encoders


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "unsafeDomains"
          , Json.Encode.list Json.Encode.string model.unsafeDomains
          )
        , ( "safeDomains"
          , Json.Encode.list Json.Encode.string model.safeDomains
          )
        , ( "exceptions", Json.Encode.list encodeException model.exceptions )
        ]


encodeException : Exception -> Json.Encode.Value
encodeException exception =
    Json.Encode.object
        [ ( "domain", Json.Encode.string exception.domain )
        , ( "endTime", Json.Encode.int <| Time.posixToMillis exception.endTime )
        ]


encodeDomainStatus : DomainStatus -> Json.Encode.Value
encodeDomainStatus status =
    Json.Encode.string <|
        case status of
            Unknown ->
                "unknown"

            Safe ->
                "safe"

            Unsafe ->
                "unsafe"


encodeMessageFromBackgroundScript : MessageFromBackgroundScript -> Json.Encode.Value
encodeMessageFromBackgroundScript message =
    case message of
        SendModel model ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "send-model" )
                , ( "model", encodeModel model )
                ]


encodeMessageToBackgroundScript : MessageToBackgroundScript -> Json.Encode.Value
encodeMessageToBackgroundScript message =
    case message of
        RequestModel ->
            Json.Encode.object [ ( "tag", Json.Encode.string "request-model" ) ]

        NewException exception ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "new-exception" )
                , ( "exception", encodeException exception )
                ]

        SetDomainStatus domain status ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "set-domain-status" )
                , ( "domain", Json.Encode.string domain )
                , ( "status", encodeDomainStatus status )
                ]



-- Decoders


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.map3 Model
        (Json.Decode.field "unsafeDomains"
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.field "safeDomains"
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.field "exceptions" (Json.Decode.list exceptionDecoder))


exceptionDecoder : Json.Decode.Decoder Exception
exceptionDecoder =
    Json.Decode.map2 Exception
        (Json.Decode.field "domain" Json.Decode.string)
        (Json.Decode.field "endTime" Json.Decode.int |> Json.Decode.map Time.millisToPosix)


domainStatusDecoder : Json.Decode.Decoder DomainStatus
domainStatusDecoder =
    let
        toStatus statusStr =
            case statusStr of
                "unknown" ->
                    Json.Decode.succeed Unknown

                "safe" ->
                    Json.Decode.succeed Safe

                "unsafe" ->
                    Json.Decode.succeed Unsafe

                _ ->
                    Json.Decode.fail "unrecognized domain status"
    in
    Json.Decode.andThen toStatus
        Json.Decode.string


messageToBackgroundScriptDecoder : Json.Decode.Decoder MessageToBackgroundScript
messageToBackgroundScriptDecoder =
    let
        decode tag =
            case tag of
                "request-model" ->
                    Json.Decode.succeed RequestModel

                "new-exception" ->
                    Json.Decode.map NewException <|
                        Json.Decode.field "exception" exceptionDecoder

                "set-domain-status" ->
                    Json.Decode.map2 SetDomainStatus
                        (Json.Decode.field "domain" Json.Decode.string)
                        (Json.Decode.field "status" domainStatusDecoder)

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


doesMatch : String -> String -> Bool
doesMatch hostname domain =
    String.endsWith domain hostname


checkDomainStatus : Model -> Url.Url -> DomainStatus
checkDomainStatus model url =
    let
        hostname =
            url.host
    in
    if model.unsafeDomains |> List.any (doesMatch hostname) then
        Unsafe

    else if model.safeDomains |> List.any (doesMatch hostname) then
        Safe

    else
        Unknown


checkIfIntercept : Model -> Url.Url -> Bool
checkIfIntercept model url =
    case checkDomainStatus model url of
        Unsafe ->
            let
                hostname =
                    url.host

                onExceptionList =
                    List.map .domain model.exceptions
                        |> List.any (doesMatch hostname)
            in
            not onExceptionList

        _ ->
            -- TODO later show a pop-up or something on unknown sites, asking user to categorize it. This logic may change.
            False
