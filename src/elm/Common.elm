module Common exposing (..)

import Json.Decode
import Json.Encode
import Time


type alias Model =
    { domainsToRedirect : List String
    , exceptions : List Exception
    }


modelDecoder =
    Json.Decode.map2 Model
        (Json.Decode.field "domainsToRedirect"
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.field "exceptions" (Json.Decode.list decodeException))


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "domainsToRedirect"
          , Json.Encode.list Json.Encode.string model.domainsToRedirect
          )
        , ( "exceptions", Json.Encode.list encodeException model.exceptions )
        ]


encodeException : Exception -> Json.Encode.Value
encodeException exception =
    Json.Encode.object
        [ ( "domain", Json.Encode.string exception.domain )
        , ( "endTime", Json.Encode.int <| Time.posixToMillis exception.endTime )
        ]


type alias Exception =
    { domain : String
    , endTime : Time.Posix
    }


type MessageFromInterceptPage
    = RequestModel
    | NewException Exception


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



-- TODO rename to messageFromInterceptPageDecoder


decodeMessageFromInterceptPage : Json.Decode.Decoder MessageFromInterceptPage
decodeMessageFromInterceptPage =
    let
        decode tag =
            case tag of
                "request-model" ->
                    Json.Decode.succeed RequestModel

                "new-exception" ->
                    Json.Decode.map NewException <|
                        Json.Decode.field "exception" decodeException

                _ ->
                    Json.Decode.fail "Unrecognized message tag"
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen decode


decodeException : Json.Decode.Decoder Exception
decodeException =
    Json.Decode.map2 Exception
        (Json.Decode.field "domain" Json.Decode.string)
        (Json.Decode.field "endTime" Json.Decode.int |> Json.Decode.map Time.millisToPosix)


type MessageFromBackgroundScript
    = SendModel Model


encodeMessageFromBackgroundScript : MessageFromBackgroundScript -> Json.Encode.Value
encodeMessageFromBackgroundScript message =
    case message of
        SendModel model ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "send-model" )
                , ( "model", encodeModel model )
                ]



-- TODO rename to messageFromInterceptPageDecoder


decodeMessageFromBackgroundScript : Json.Decode.Decoder MessageFromBackgroundScript
decodeMessageFromBackgroundScript =
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
