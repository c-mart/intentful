port module Intercept exposing (..)

import Background exposing (Msg(..))
import Browser
import Browser.Navigation
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



-- Primary types


type AppValidity
    = AppValid Model
    | AppInvalid String


type alias Model =
    { common : C.Model
    , nextUrl : Url.Url
    , pageLoadTime : Time.Posix
    , currentTime : Time.Posix
    , viewState : ViewState
    }


type Msg
    = ReceiveMessage Json.Encode.Value
    | ReceiveCurrentTime Time.Posix
    | GotCloseCurrentTab
    | GotAdvanceToCreateException
    | GotCreateException Url.Url ExceptionEndTime
    | GotExceptionDurationInput String



-- Helper types


type ViewState
    = InitialView
    | CreatingException ExceptionDurationInput


type alias ExceptionDurationInput =
    String


type ExceptionCreatability
    = CanCreate MinutesInt
    | WaitToCreate TimeRemainingMillis
    | InvalidDuration


type alias TimeRemainingMillis =
    Int


type alias MinutesInt =
    Int


type alias ExceptionEndTime =
    Time.Posix



-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port closeCurrentTab : () -> Cmd msg



-- Primary functions


main : Program Json.Encode.Value AppValidity Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> subs
        }


init : Json.Decode.Value -> ( AppValidity, Cmd Msg )
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

        commonModel =
            Json.Decode.decodeValue (Json.Decode.field "common-model" C.modelDecoder) flags
    in
    case ( commonModel, nextUrl ) of
        ( Ok m, Ok u ) ->
            ( AppValid <| Model m u time time InitialView, Cmd.none )

        ( Err decodeErr, _ ) ->
            ( AppInvalid (Json.Decode.errorToString decodeErr), Cmd.none )

        ( _, Err nextUrlErr ) ->
            ( AppInvalid nextUrlErr, Cmd.none )


update : Msg -> AppValidity -> ( AppValidity, Cmd Msg )
update msg validity =
    case validity of
        AppValid model ->
            updateValid msg model
                |> Tuple.mapFirst AppValid

        AppInvalid _ ->
            ( validity, Cmd.none )


updateValid : Msg -> Model -> ( Model, Cmd Msg )
updateValid msg model =
    case msg of
        ReceiveMessage value ->
            case Json.Decode.decodeValue C.messageFromBackgroundScriptDecoder value of
                Ok message ->
                    case message of
                        C.SendModel commonModel ->
                            ( { model | common = commonModel }
                            , if C.checkIfIntercept commonModel model.nextUrl then
                                Cmd.none

                              else
                                Browser.Navigation.load (Url.toString model.nextUrl)
                            )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )

        ReceiveCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        GotCloseCurrentTab ->
            ( model, closeCurrentTab () )

        GotAdvanceToCreateException ->
            ( { model | viewState = CreatingException "1" }, Cmd.none )

        GotExceptionDurationInput minsStr ->
            case model.viewState of
                InitialView ->
                    -- Impossible state?
                    ( model, Cmd.none )

                CreatingException _ ->
                    ( { model | viewState = CreatingException minsStr }, Cmd.none )

        GotCreateException url endTime ->
            let
                exception =
                    C.encodeMessageToBackgroundScript
                        (C.NewException <|
                            C.Exception (C.Hostname url.host) endTime
                        )
            in
            ( model, sendMessage exception )


view : AppValidity -> Html Msg
view validity =
    let
        showErr errStr =
            Html.text ("Cannot render page: " ++ errStr)
    in
    case validity of
        AppValid model ->
            viewValid model

        AppInvalid e ->
            showErr e


subs : Sub Msg
subs =
    Sub.batch
        [ receiveMessage ReceiveMessage
        , Time.every 1000 ReceiveCurrentTime
        ]



-- Helper functions


viewValid : Model -> Html Msg
viewValid model =
    case model.viewState of
        InitialView ->
            viewInitial model

        CreatingException durationInput ->
            viewCreatingException model durationInput


viewInitial : Model -> Html Msg
viewInitial model =
    Html.div []
        [ Html.p []
            [ Html.text
                (model.nextUrl.host
                    ++ " is an unsafe site."
                )
            ]
        , Html.ul []
            [ Html.li [ HtmlE.onClick GotAdvanceToCreateException ] [ Html.button [] [ Html.text "Proceed anyway" ] ]
            , Html.li []
                [ Html.button
                    [ HtmlE.onClick GotCloseCurrentTab
                    ]
                    [ Html.text "Close the tab, I don't need to go here" ]
                ]
            ]
        ]


viewCreatingException : Model -> ExceptionDurationInput -> Html Msg
viewCreatingException model durationInput =
    let
        createExceptionButton =
            case canCreateException model durationInput of
                CanCreate durationMins ->
                    let
                        expireTime =
                            model.currentTime
                                |> Time.posixToMillis
                                |> (+) (durationMins * 60 * 1000)
                                |> Time.millisToPosix
                    in
                    Html.button [ HtmlE.onClick (GotCreateException model.nextUrl expireTime) ] [ Html.text ("Go to " ++ model.nextUrl.host) ]

                WaitToCreate waitRemainMillis ->
                    Html.button [] [ Html.text <| "You must wait " ++ countdownRemainText waitRemainMillis ]

                InvalidDuration ->
                    Html.button [] [ Html.text "Breaux, you must enter a number of minutes" ]
    in
    Html.div []
        [ Html.p []
            [ Html.text
                ("You were going to "
                    ++ model.nextUrl.host
                    ++ ", an unsafe site."
                )
            ]
        , Html.ul []
            [ Html.li []
                [ Html.span []
                    [ Html.text ("Enable " ++ model.nextUrl.host ++ " for")
                    , Html.input
                        [ HtmlE.onInput GotExceptionDurationInput
                        , HtmlA.value durationInput
                        ]
                        []
                    , Html.text "minutes"
                    ]
                ]
            , Html.li [] [ createExceptionButton ]
            , Html.li []
                [ Html.button
                    [ HtmlE.onClick GotCloseCurrentTab
                    ]
                    [ Html.text "Close the tab, I don't need to go here" ]
                ]
            ]
        ]


canCreateException : Model -> ExceptionDurationInput -> ExceptionCreatability
canCreateException model durationInput =
    let
        waitDurationMillis =
            -- TODO start this when someone advances to create exception, not when intercept page loads
            3 * 1000 - 1

        waitRemainMillis =
            waitDurationMillis - (Time.posixToMillis model.currentTime - Time.posixToMillis model.pageLoadTime)
    in
    if waitRemainMillis > 0 then
        WaitToCreate waitRemainMillis

    else
        case String.toInt durationInput of
            Just i ->
                CanCreate i

            Nothing ->
                InvalidDuration


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
