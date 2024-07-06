port module Intercept exposing (..)

import Background exposing (Msg(..))
import Browser
import Browser.Navigation
import Common as C
import Debug
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlA
import Html.Styled.Events as HtmlE
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
    , currentTime : Time.Posix
    , viewState : ViewState
    }


type Msg
    = ReceiveMessage Json.Encode.Value
    | ReceiveCurrentTime Time.Posix
    | GotCloseCurrentTab
    | GotAdvanceToCreateException
    | GotCreateException Url.Url String ExceptionEndTime
    | GotExceptionReasonInput String
    | GotExceptionDurationInput String



-- Helper types


type ViewState
    = InitialView
    | CreatingException CreatingExceptionParams


type alias CreatingExceptionParams =
    { timeEnteredForm : Time.Posix
    , reasonInput : ExceptionReasonInput
    , durationInput : ExceptionDurationInput
    }


type alias ExceptionReasonInput =
    String


type alias ExceptionDurationInput =
    String


type ExceptionCreatability
    = CanCreate MinutesInt
    | WaitToCreate TimeRemainingMillis
    | InvalidReason
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
        , view = view >> Html.toUnstyled
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
            ( AppValid <| Model m u time InitialView, Cmd.none )

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
            ( { model | viewState = CreatingException (initCreatingExceptionParams model.currentTime) }, Cmd.none )

        GotExceptionReasonInput reasonStr ->
            case model.viewState of
                InitialView ->
                    -- Impossible state?
                    ( model, Cmd.none )

                CreatingException params ->
                    ( { model | viewState = CreatingException { params | reasonInput = reasonStr } }, Cmd.none )

        GotExceptionDurationInput minsStr ->
            case model.viewState of
                InitialView ->
                    -- Impossible state?
                    ( model, Cmd.none )

                CreatingException params ->
                    ( { model | viewState = CreatingException { params | durationInput = minsStr } }, Cmd.none )

        GotCreateException url reason endTime ->
            let
                exception =
                    C.encodeMessageToBackgroundScript
                        (C.NewException <|
                            C.Exception (C.Hostname url.host) reason endTime
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


initCreatingExceptionParams : Time.Posix -> CreatingExceptionParams
initCreatingExceptionParams timeEnteredForm =
    { timeEnteredForm = timeEnteredForm
    , reasonInput = ""
    , durationInput = "1"
    }


viewValid : Model -> Html Msg
viewValid model =
    Html.div []
        [ case model.common.mode of
            C.TestMode expireTime ->
                C.testModeBanner (Time.millisToPosix 0) expireTime

            _ ->
                -- TODO consider Html.none?
                Html.text ""
        , Html.div [ HtmlA.class "intercept-dialog" ]
            (case model.viewState of
                InitialView ->
                    viewInitial model

                CreatingException params ->
                    viewCreatingException model params
            )
        ]


viewInitial : Model -> List (Html Msg)
viewInitial model =
    [ Html.h1 []
        [ Html.text
            (model.nextUrl.host
                ++ " is an unsafe site."
            )
        ]
    , Html.form
        [ HtmlA.class "intercept-dialog-bottom"
        , HtmlA.class "button-row"
        ]
        [ Html.p []
            [ Html.button
                [ HtmlA.type_ "button"
                , HtmlE.onClick GotAdvanceToCreateException
                ]
                [ Html.text "Proceed anyway" ]
            ]
        , Html.p []
            [ Html.button
                [ HtmlA.type_ "button"
                , HtmlE.onClick GotCloseCurrentTab
                ]
                [ Html.text "Close the tab, I don't need to go here" ]
            ]
        ]
    ]


viewCreatingException : Model -> CreatingExceptionParams -> List (Html Msg)
viewCreatingException model params =
    let
        createExceptionButton =
            case canCreateException model params of
                CanCreate durationMins ->
                    let
                        expireTime =
                            model.currentTime
                                |> Time.posixToMillis
                                |> (+) (durationMins * 60 * 1000)
                                |> Time.millisToPosix
                    in
                    Html.button [ HtmlA.type_ "button", HtmlE.onClick (GotCreateException model.nextUrl params.reasonInput expireTime) ]
                        [ Html.text ("Go to " ++ model.nextUrl.host) ]

                WaitToCreate waitRemainMillis ->
                    Html.text <| "You must wait " ++ countdownRemainText waitRemainMillis

                InvalidReason ->
                    Html.text "Provide a longer reason for using the unsafe site"

                InvalidDuration ->
                    Html.text "Breaux, you must enter a number of minutes"
    in
    [ Html.p []
        [ Html.text
            ("You were going to "
                ++ model.nextUrl.host
                ++ ", an unsafe site."
            )
        ]
    , Html.p []
        [ Html.label [ HtmlA.for "exception-reason-input" ]
            [ Html.text ("Why do you want to use " ++ model.nextUrl.host ++ " now?") ]
        , Html.input
            [ HtmlE.onInput GotExceptionReasonInput
            , HtmlA.value params.reasonInput
            , HtmlA.id "exception-reason-input"
            ]
            []
        ]
    , Html.p []
        [ Html.label [ HtmlA.for "exception-duration-input" ]
            [ Html.text ("Enable " ++ model.nextUrl.host ++ " for")
            ]
        , Html.input
            [ HtmlE.onInput GotExceptionDurationInput
            , HtmlA.value params.durationInput
            , HtmlA.id "exception-duration-input"
            ]
            []
        , Html.text "minutes"
        ]
    , Html.p
        [ HtmlA.class "intercept-dialog-bottom"
        , HtmlA.class "button-row"
        ]
        [ Html.div []
            [ createExceptionButton ]
        , Html.div []
            [ Html.button
                [ HtmlE.onClick GotCloseCurrentTab
                , HtmlA.type_ "button"
                ]
                [ Html.text "Close the tab, I don't need to go here" ]
            ]
        ]
    ]


canCreateException : Model -> CreatingExceptionParams -> ExceptionCreatability
canCreateException model params =
    let
        waitDurationMillis =
            let
                seconds =
                    case model.common.mode of
                        C.NormalMode ->
                            30

                        C.TestMode _ ->
                            3
            in
            seconds * 1000 - 1

        waitRemainMillis =
            waitDurationMillis - (Time.posixToMillis model.currentTime - Time.posixToMillis params.timeEnteredForm)

        reasonValid =
            String.length params.reasonInput > 20
    in
    if waitRemainMillis > 0 then
        WaitToCreate waitRemainMillis

    else
        case ( String.toInt params.durationInput, reasonValid ) of
            ( Just i, True ) ->
                CanCreate i

            ( _, False ) ->
                InvalidReason

            ( Nothing, _ ) ->
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
