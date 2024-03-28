module Common exposing (..)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as HtmlA
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


type Hostname
    = Hostname String


unwrapHostname : Hostname -> String
unwrapHostname (Hostname h) =
    h


type alias Model =
    { tabs : List Tab
    , unsafeSites : List Hostname
    , safeSites : List Hostname
    , exceptions : List Exception
    , mode : Mode
    }


type alias Tab =
    { id : Int
    , url : String
    }


type alias NavigationEvent =
    { tabId : Int
    , url : String
    , frameId : Int
    }


type alias Exception =
    { site : Hostname
    , reason : String
    , endTime : Time.Posix
    }


type Mode
    = NormalMode
    | TestMode


type SiteStatus
    = Unknown
    | Safe
    | Unsafe


type MessageFromBackgroundScript
    = SendModel Model


type MessageToBackgroundScript
    = RequestModel
    | NewException Exception
    | SetSiteStatus Hostname SiteStatus



-- Encoders


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "tabs", Json.Encode.list encodeTab model.tabs )
        , ( "unsafeSites"
          , Json.Encode.list
                Json.Encode.string
                (List.map unwrapHostname model.unsafeSites)
          )
        , ( "safeSites"
          , Json.Encode.list
                Json.Encode.string
                (List.map unwrapHostname model.safeSites)
          )
        , ( "exceptions", Json.Encode.list encodeException model.exceptions )
        , ( "mode", Json.Encode.string (modeToStr model.mode) )
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
        [ ( "site", Json.Encode.string (unwrapHostname exception.site) )
        , ( "reason", Json.Encode.string exception.reason )
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

        SetSiteStatus hostname status ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "set-site-status" )
                , ( "site", Json.Encode.string (unwrapHostname hostname) )
                , ( "status", encodeSiteStatus status )
                ]


encodeMode : Mode -> Json.Encode.Value
encodeMode mode =
    mode |> modeToStr |> Json.Encode.string



-- Decoders


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.map5 Model
        (Json.Decode.field "tabs" (Json.Decode.list tabDecoder))
        (Json.Decode.field "unsafeSites"
            (Json.Decode.list (Json.Decode.string |> Json.Decode.map Hostname))
        )
        (Json.Decode.field "safeSites"
            (Json.Decode.list (Json.Decode.string |> Json.Decode.map Hostname))
        )
        (Json.Decode.field "exceptions" (Json.Decode.list exceptionDecoder))
        (Json.Decode.field "mode" modeDecoder)


tabDecoder : Json.Decode.Decoder Tab
tabDecoder =
    Json.Decode.map2 Tab
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)


exceptionDecoder : Json.Decode.Decoder Exception
exceptionDecoder =
    Json.Decode.map3 Exception
        (Json.Decode.field "site" Json.Decode.string |> Json.Decode.map Hostname)
        (Json.Decode.field "reason" Json.Decode.string)
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


modeDecoder : Json.Decode.Decoder Mode
modeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case modeFromStr str of
                    Ok mode ->
                        Json.Decode.succeed mode

                    Err errStr ->
                        Json.Decode.fail errStr
            )


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
                        (Json.Decode.field "site" Json.Decode.string |> Json.Decode.map Hostname)
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


hostnameToRegisteredDomain : Hostname -> RegisteredDomain
hostnameToRegisteredDomain (Hostname hostname) =
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


hostnameIncludes : Hostname -> Hostname -> Bool
hostnameIncludes (Hostname subHostname) (Hostname superHostname) =
    String.endsWith superHostname subHostname


checkSiteStatus : Model -> Url.Url -> SiteStatus
checkSiteStatus model url =
    let
        hostname =
            Hostname url.host
    in
    if
        model.unsafeSites
            |> List.any (hostnameIncludes hostname)
    then
        Unsafe

    else if
        model.safeSites
            |> List.any (hostnameIncludes hostname)
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
                    Hostname url.host

                onExceptionList =
                    List.map .site model.exceptions
                        |> List.any (hostnameIncludes hostname)
            in
            not onExceptionList

        _ ->
            -- TODO later show a pop-up or something on unknown sites, asking user to categorize it. This logic may change.
            False


modeToStr : Mode -> String
modeToStr mode =
    case mode of
        NormalMode ->
            "normalMode"

        TestMode ->
            "testMode"


modeFromStr : String -> Result String Mode
modeFromStr str =
    case str of
        "normalMode" ->
            Ok NormalMode

        "testMode" ->
            Ok TestMode

        _ ->
            Err ("unrecognized string " ++ str ++ " for application mode")



-- UI code


testModeBanner : Html.Html msg
testModeBanner =
    Html.strong [ HtmlA.css [ Css.color (Css.rgb 255 0 0) ] ] [ Html.text "Danger: Test Mode" ]
