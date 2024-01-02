port module Intercept exposing (..)

import Background exposing (Msg(..))
import Browser
import Common as C
import Debug
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode
import Json.Encode
import Time
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query


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
    let
        hrefStr =
            Json.Decode.decodeValue (Json.Decode.field "href" Json.Decode.string) flags
                |> Result.mapError Json.Decode.errorToString

        hrefStrToNextUrlStr hrefStr_ =
            let
                hrefSchemeSwap =
                    -- https://github.com/elm/url/issues/10
                    hrefStr_
                        |> String.replace "moz-extension" "http"
                        |> String.replace "chrome-extension" "http"

                qParser =
                    Url.Parser.Query.string "next"

                parseQ url =
                    Url.Parser.parse (Url.Parser.s "intercept.html" <?> qParser) url
            in
            case Url.fromString hrefSchemeSwap of
                Just url ->
                    case parseQ url of
                        Just (Just nextUrl_) ->
                            Ok nextUrl_

                        _ ->
                            Err "Could not parse query string"

                Nothing ->
                    Err "Could not parse next URL"

        nextUrl =
            case Result.andThen hrefStrToNextUrlStr hrefStr of
                Ok uS ->
                    case Url.fromString uS of
                        Just url ->
                            Ok url

                        Nothing ->
                            Err "Could not parse URL in \"next\" query parameter"

                Err e ->
                    Err e

        time =
            Json.Decode.decodeValue (Json.Decode.field "epoch" Json.Decode.int) flags
                -- TODO maybe handle the error case in a better way
                |> Result.withDefault 0
                |> Time.millisToPosix
    in
    ( Model Waiting nextUrl time time "2", Cmd.none )


type alias Model =
    { common : CommonModel
    , nextUrl : Result String Url.Url
    , pageLoadTime : Time.Posix
    , currentTime : Time.Posix
    , exceptionDurationInput : String
    }


type CommonModel
    = Valid C.Model
    | Invalid String
    | Waiting


type alias ResolvedModel =
    { common : C.Model
    , nextUrl : Url.Url
    , pageLoadTime : Time.Posix
    , currentTime : Time.Posix
    , exceptionDurationInput : String
    }


type ExceptionCreatability
    = CanCreate MinutesInt
    | WaitToCreate TimeRemainingMillis
    | InvalidDuration


type alias TimeRemainingMillis =
    Int


canCreateException : ResolvedModel -> ExceptionCreatability
canCreateException rm =
    let
        waitDurationMillis =
            -- 10 seconds
            10 * 1000 - 1

        waitRemainMillis =
            waitDurationMillis - (Time.posixToMillis rm.currentTime - Time.posixToMillis rm.pageLoadTime)
    in
    if waitRemainMillis > 0 then
        WaitToCreate waitRemainMillis

    else
        case String.toInt rm.exceptionDurationInput of
            Just i ->
                CanCreate i

            Nothing ->
                InvalidDuration


toResolvedModel : Model -> Result String ResolvedModel
toResolvedModel model =
    case ( model.common, model.nextUrl ) of
        ( Valid commonModel, Ok url ) ->
            Ok
                (ResolvedModel
                    commonModel
                    url
                    model.pageLoadTime
                    model.currentTime
                    model.exceptionDurationInput
                )

        ( Invalid invalidErr, _ ) ->
            Err ("Cannot resolve because common model is invalid: " ++ invalidErr)

        ( Waiting, _ ) ->
            Err "Cannot resolve because waiting for common model"

        ( _, Err urlErr ) ->
            Err ("Cannot resolve because next URL is invalid: " ++ urlErr)


decodeCommonModel : Json.Decode.Value -> CommonModel
decodeCommonModel flags =
    case Json.Decode.decodeValue C.modelDecoder flags of
        Ok commonModel ->
            Valid commonModel

        Err e ->
            Invalid (Json.Decode.errorToString e)


type Msg
    = ReceiveMessage Json.Encode.Value
    | ReceiveCurrentTime Time.Posix
    | GotCreateException Url.Url ExceptionEndTime
    | GotExceptionDurationInput String


type alias MinutesInt =
    Int


type alias ExceptionEndTime =
    Time.Posix


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        ReceiveMessage value ->
            case Json.Decode.decodeValue C.decodeMessageFromBackgroundScript value of
                Ok message ->
                    case message of
                        C.SendModel commonModel ->
                            ( { model | common = Valid commonModel }, Cmd.none )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )

        ReceiveCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        GotExceptionDurationInput minsStr ->
            ( { model | exceptionDurationInput = minsStr }, Cmd.none )

        GotCreateException url endTime ->
            let
                exception =
                    C.encodeMessageFromInterceptPage
                        (C.NewException <|
                            C.Exception url.host endTime
                        )
            in
            ( model, sendMessage exception )


view : Model -> Html Msg
view model =
    case toResolvedModel model of
        Ok resolvedModel ->
            viewResolved resolvedModel

        Err errStr ->
            Html.text ("Cannot render page: " ++ errStr)


viewResolved : ResolvedModel -> Html Msg
viewResolved rm =
    let
        createExceptionButton =
            case canCreateException rm of
                CanCreate durationMins ->
                    let
                        expireTime =
                            rm.currentTime
                                |> Time.posixToMillis
                                |> (+) (durationMins * 60 * 1000)
                                |> Time.millisToPosix
                    in
                    Html.button [ HtmlE.onClick (GotCreateException rm.nextUrl expireTime) ] [ Html.text "Create exception" ]

                WaitToCreate waitRemainMillis ->
                    Html.button [] [ Html.text <| "You must wait " ++ countdownRemainText waitRemainMillis ++ " to create an exception" ]

                InvalidDuration ->
                    Html.button [] [ Html.text "Breaux, you must enter a number of minutes" ]
    in
    Html.div []
        [ Html.p []
            [ Html.text
                ("You were going to "
                    ++ rm.nextUrl.host
                )
            ]
        , Html.ul []
            [ Html.li []
                [ Html.input
                    [ HtmlE.onInput GotExceptionDurationInput
                    , HtmlA.value rm.exceptionDurationInput
                    ]
                    []
                ]
            , Html.li [] [ createExceptionButton ]
            ]
        ]


countdownRemainText : Int -> String
countdownRemainText millis =
    let
        secs =
            millis // 1000 + 1

        toPadStr i =
            i |> String.fromInt |> String.padLeft 2 '0'

        minsField =
            secs // 60 |> toPadStr

        secsField =
            remainderBy 60 secs |> toPadStr
    in
    minsField ++ ":" ++ secsField


subs : Sub Msg
subs =
    Sub.batch
        [ receiveMessage ReceiveMessage
        , Time.every 1000 ReceiveCurrentTime
        ]


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg
