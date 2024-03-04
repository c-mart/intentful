module Common exposing (..)

import Json.Decode
import Json.Encode
import PSL
import Set
import Time
import Url



-- Types


type alias Model =
    { tabs : List Tab
    , unsafeDomains : Set.Set String
    , safeDomains : Set.Set String
    , exceptions : List Exception
    }


type alias Tab =
    { id : Int
    , url : String
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
        [ ( "tabs", Json.Encode.list encodeTab model.tabs )
        , ( "unsafeDomains"
          , Json.Encode.list
                Json.Encode.string
                (Set.toList model.unsafeDomains)
          )
        , ( "safeDomains"
          , Json.Encode.list
                Json.Encode.string
                (Set.toList model.safeDomains)
          )
        , ( "exceptions", Json.Encode.list encodeException model.exceptions )
        ]


encodeTab : Tab -> Json.Encode.Value
encodeTab tab =
    Json.Encode.object
        [ ( "id", Json.Encode.int tab.id )
        , ( "url", Json.Encode.string tab.url )
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
    Json.Decode.map4 Model
        (Json.Decode.field "tabs" (Json.Decode.list tabDecoder))
        (Json.Decode.field "unsafeDomains"
            (Json.Decode.list Json.Decode.string
                |> Json.Decode.map Set.fromList
            )
        )
        (Json.Decode.field "safeDomains"
            (Json.Decode.list Json.Decode.string
                |> Json.Decode.map Set.fromList
            )
        )
        (Json.Decode.field "exceptions" (Json.Decode.list exceptionDecoder))


tabDecoder : Json.Decode.Decoder Tab
tabDecoder =
    Json.Decode.map2 Tab
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)


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


hostnameToRegisteredDomain : String -> String
hostnameToRegisteredDomain hostname =
    let
        maybeSuffix =
            PSL.list
                |> List.filter (\suffix_ -> String.endsWith suffix_ hostname)
                -- Return longest matching suffix? Not sure about this.
                |> List.sortBy (\s -> 0 - String.length s)
                |> List.head
    in
    case maybeSuffix of
        Nothing ->
            -- No match, just return the whole thing
            hostname

        Just suffix ->
            let
                -- This factoring is kind of garbage
                suffixLength =
                    String.length suffix

                hostnameNoSuffix =
                    String.dropRight
                        -- Also drop the period preceding suffix
                        (suffixLength + 1)
                        hostname

                labelPriorToSuffix =
                    hostnameNoSuffix
                        |> String.split "."
                        |> List.reverse
                        |> List.head
                        -- This should be an impossible state
                        |> Maybe.withDefault ""
            in
            labelPriorToSuffix ++ "." ++ suffix


doesMatch : String -> String -> Bool
doesMatch hostname domain =
    String.endsWith domain hostname


checkDomainStatus : Model -> Url.Url -> DomainStatus
checkDomainStatus model url =
    let
        hostname =
            url.host
    in
    if
        model.unsafeDomains
            |> Set.toList
            |> List.any (doesMatch hostname)
    then
        Unsafe

    else if
        model.safeDomains
            |> Set.toList
            |> List.any (doesMatch hostname)
    then
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
