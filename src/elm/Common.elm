module Common exposing (..)

import Json.Decode
import Json.Encode


type alias Model =
    { domainsToRedirect : List String
    }


modelDecoder =
    Json.Decode.map Model
        (Json.Decode.field "domainsToRedirect"
            (Json.Decode.list Json.Decode.string)
        )


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "domainsToRedirect"
          , Json.Encode.list Json.Encode.string model.domainsToRedirect
          )
        ]


type alias Exception =
    { domain : String }


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
                , ( "exception"
                  , Json.Encode.object
                        [ ( "domain", Json.Encode.string exception.domain ) ]
                  )
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
                    Json.Decode.at [ "exception", "domain" ] Json.Decode.string
                        |> Json.Decode.map Exception
                        |> Json.Decode.map NewException

                _ ->
                    Json.Decode.fail "Unrecognized message tag"
    in
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen decode


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
