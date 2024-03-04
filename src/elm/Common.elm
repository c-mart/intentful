module Common exposing (..)

import Json.Decode
import Json.Encode
import PSL
import Set
import Time
import Url



-- Constants


unsafeSitesForDevTesting : List String
unsafeSitesForDevTesting =
    [ "reddit.com"
    , "redd.it"
    , "facebook.com"
    , "washingtonpost.com"
    , "patreon.com"
    , "ebay.com"
    , "craigslist.org"
    , "ycombinator.com"
    , "nytimes.com"
    , "politico.com"
    , "news.google.com"
    , "cnn.com"
    , "fivethirtyeight.com"
    , "twitter.com"
    , "talkingpointsmemo.com"
    , "axios.com"
    , "apnews.com"
    , "foxnews.com"
    , "theonion.com"
    , "twitter.com"
    , "msnbc.com"
    , "mtbr.com"
    , "abcnews.com"
    , "npr.org"
    , "strava.com"
    , "yahoo.com"
    , "reuters.com"
    , "slatestarcodex.com"
    , "astralcodexten.substack.com"
    , "astralcodexten.com"
    , "lesswrong.com"
    , "thezvi.substack.com"
    , "thezvi.wordpress.com"
    , "wikipedia.org"
    , "nsmb.com"
    , "meatengines.com"
    , "radpowerbikes.com"
    , "radpowerbikes.zendesk.com"
    , "amazon.com"
    , "cbsnews.com"
    , "nbcnews.com"
    , "frame.work"
    ]



-- Types


type RegisteredDomain
    = RegisteredDomain String


unwrapRegisteredDomain : RegisteredDomain -> String
unwrapRegisteredDomain (RegisteredDomain rDom) =
    rDom


type alias Model =
    { tabs : List Tab
    , unsafeSites : List String
    , safeSites : List String
    , exceptions : List Exception
    }


type alias Tab =
    { id : Int
    , url : String
    }


type alias Exception =
    { site : RegisteredDomain
    , endTime : Time.Posix
    }


type SiteStatus
    = Unknown
    | Safe
    | Unsafe


type MessageFromBackgroundScript
    = SendModel Model


type MessageToBackgroundScript
    = RequestModel
    | NewException Exception
    | SetSiteStatus RegisteredDomain SiteStatus



-- Encoders


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "tabs", Json.Encode.list encodeTab model.tabs )
        , ( "unsafeSites"
          , Json.Encode.list
                Json.Encode.string
                model.unsafeSites
          )
        , ( "safeSites"
          , Json.Encode.list
                Json.Encode.string
                model.safeSites
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
        [ ( "site", Json.Encode.string (unwrapRegisteredDomain exception.site) )
        , ( "endTime", Json.Encode.int <| Time.posixToMillis exception.endTime )
        ]


encodeSiteStatus : SiteStatus -> Json.Encode.Value
encodeSiteStatus status =
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

        SetSiteStatus domain status ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "set-site-status" )
                , ( "site", Json.Encode.string (unwrapRegisteredDomain domain) )
                , ( "status", encodeSiteStatus status )
                ]



-- Decoders


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.map4 Model
        (Json.Decode.field "tabs" (Json.Decode.list tabDecoder))
        (Json.Decode.field "unsafeSites"
            (Json.Decode.list Json.Decode.string)
        )
        (Json.Decode.field "safeSites"
            (Json.Decode.list Json.Decode.string)
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
        (Json.Decode.field "site" Json.Decode.string |> Json.Decode.map RegisteredDomain)
        (Json.Decode.field "endTime" Json.Decode.int |> Json.Decode.map Time.millisToPosix)


siteStatusDecoder : Json.Decode.Decoder SiteStatus
siteStatusDecoder =
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
                    Json.Decode.fail "unrecognized site status"
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

                "set-site-status" ->
                    Json.Decode.map2 SetSiteStatus
                        (Json.Decode.field "site" Json.Decode.string |> Json.Decode.map RegisteredDomain)
                        (Json.Decode.field "status" siteStatusDecoder)

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


hostnameToRegisteredDomain : String -> RegisteredDomain
hostnameToRegisteredDomain hostname =
    let
        maybeSuffix =
            PSL.list
                |> List.filter (\suffix_ -> String.endsWith suffix_ hostname)
                -- Return longest matching suffix? Not sure about this.
                |> List.sortBy (\s -> 0 - String.length s)
                |> List.head
    in
    RegisteredDomain <|
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


doesMatch : String -> RegisteredDomain -> Bool
doesMatch hostname (RegisteredDomain rDom) =
    String.endsWith rDom hostname


checkSiteStatus : Model -> Url.Url -> SiteStatus
checkSiteStatus model url =
    let
        hostname =
            url.host
    in
    if
        model.unsafeSites
            -- Ew, fix
            |> List.map RegisteredDomain
            |> List.any (doesMatch hostname)
    then
        Unsafe

    else if
        model.safeSites
            -- Ew, fix
            |> List.map RegisteredDomain
            |> List.any (doesMatch hostname)
    then
        Safe

    else
        Unknown


checkIfIntercept : Model -> Url.Url -> Bool
checkIfIntercept model url =
    case checkSiteStatus model url of
        Unsafe ->
            let
                hostname =
                    url.host

                onExceptionList =
                    List.map .site model.exceptions
                        |> List.any (doesMatch hostname)
            in
            not onExceptionList

        _ ->
            -- TODO later show a pop-up or something on unknown sites, asking user to categorize it. This logic may change.
            False
