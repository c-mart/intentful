port module Background exposing (..)

import Common as C exposing (RegisteredDomain)
import Debug
import Html exposing (a)
import Json.Decode
import Json.Encode
import Set
import Task
import Time
import Url



-- Primary types


type Msg
    = GotUrlChange Json.Encode.Value
    | GotMessage Json.Encode.Value
    | GotCurrentTime Time.Posix
    | GotTabs Json.Encode.Value
    | GotAlarm Json.Encode.Value



-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port getUrlChange : (Json.Encode.Value -> msg) -> Sub msg


port setRedirect : Json.Encode.Value -> Cmd msg


port requestTabs : () -> Cmd msg


port receiveTabs : (Json.Encode.Value -> msg) -> Sub msg


port setStorage : Json.Encode.Value -> Cmd msg


port consoleLog : String -> Cmd msg


port receiveAlarm : (Json.Encode.Value -> msg) -> Sub msg



-- Primary functions


main : Program Json.Decode.Value C.Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subs
        }


init : Json.Decode.Value -> ( C.Model, Cmd msg )
init flags =
    let
        storedModelResult =
            Json.Decode.decodeValue (Json.Decode.field "storedState" C.modelDecoder) flags

        model =
            storedModelResult
                |> Result.withDefault
                    -- Empty model because could not decode local storage
                    { tabs = []
                    , unsafeDomains =
                        [ "weather.gov"
                        ]
                    , safeDomains =
                        []
                    , exceptions = []
                    }
    in
    ( model
    , Cmd.batch [ requestTabs (), setStorage (C.encodeModel model) ]
    )


update : Msg -> C.Model -> ( C.Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            innerUpdate msg model
    in
    ( newModel
    , Cmd.batch
        [ cmd
        , if newModel /= model then
            setStorage (C.encodeModel newModel)

          else
            Cmd.none
        , sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel newModel)
        ]
    )


subs _ =
    Sub.batch
        [ getUrlChange GotUrlChange
        , receiveMessage GotMessage
        , receiveTabs GotTabs
        , receiveAlarm GotAlarm
        , Time.every 5000 GotCurrentTime
        ]



-- Helper functions


innerUpdate : Msg -> C.Model -> ( C.Model, Cmd Msg )
innerUpdate msg model =
    case msg of
        GotMessage value ->
            case Json.Decode.decodeValue C.messageToBackgroundScriptDecoder value of
                Ok message ->
                    case message of
                        C.RequestModel ->
                            ( model, sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel model) )

                        C.NewException exception ->
                            ( { model | exceptions = exception :: model.exceptions }, Cmd.none )

                        C.SetDomainStatus rDom status ->
                            setDomainStatus model rDom status

                Err e ->
                    ( model
                    , logJsonError "Could not decode message" e
                    )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok tab ->
                    let
                        newModel =
                            modelUpdateTab model tab
                    in
                    ( newModel, processTab newModel tab )

                Err e ->
                    ( model
                    , logJsonError "Could not decode URL change" e
                    )

        GotAlarm _ ->
            ( model, Task.perform GotCurrentTime Time.now )

        GotCurrentTime time ->
            let
                keepException : C.Exception -> Bool
                keepException e =
                    Time.posixToMillis e.endTime > Time.posixToMillis time

                newExceptions =
                    List.filter keepException model.exceptions

                cmd =
                    if newExceptions /= model.exceptions then
                        requestTabs ()

                    else
                        Cmd.none
            in
            ( { model | exceptions = newExceptions }, cmd )

        GotTabs tabsJson ->
            case decodeTabs tabsJson of
                Ok tabs ->
                    ( { model | tabs = tabs }
                    , Cmd.batch <| List.map (processTab model) tabs
                    )

                Err e ->
                    ( model
                    , logJsonError "Could not decode tabs" e
                    )


setDomainStatus : C.Model -> C.RegisteredDomain -> C.DomainStatus -> ( C.Model, Cmd Msg )
setDomainStatus model (C.RegisteredDomain domain) status =
    let
        insert : a -> List a -> List a
        insert item list =
            if List.member item list then
                list

            else
                item :: list
    in
    case status of
        C.Unknown ->
            ( { model
                | unsafeDomains =
                    List.filter (\d -> d /= domain) model.unsafeDomains
                , safeDomains =
                    List.filter (\d -> d /= domain) model.safeDomains
              }
            , Cmd.none
            )

        C.Safe ->
            ( { model
                | unsafeDomains =
                    List.filter (\d -> d /= domain) model.unsafeDomains
                , safeDomains =
                    insert domain model.safeDomains
              }
            , Cmd.none
            )

        C.Unsafe ->
            let
                newModel =
                    { model
                        | unsafeDomains =
                            insert domain model.unsafeDomains
                        , safeDomains =
                            List.filter (\d -> d /= domain) model.safeDomains
                    }
            in
            ( newModel
            , List.map (processTab newModel) newModel.tabs |> Cmd.batch
            )


logJsonError : String -> Json.Decode.Error -> Cmd msg
logJsonError description e =
    let
        eStr =
            description ++ ": " ++ Json.Decode.errorToString e
    in
    consoleLog eStr


modelUpdateTab : C.Model -> C.Tab -> C.Model
modelUpdateTab model newTab =
    let
        otherTabs =
            model.tabs |> List.filter (\t -> t.id /= newTab.id)
    in
    { model | tabs = newTab :: otherTabs }


processTab : C.Model -> C.Tab -> Cmd msg
processTab model tab =
    -- TODO if this is always called for the whole model at once, maybe act on all tabs in the model, never for one at a time
    -- TODO rename this to tabIssueRedirect, or something
    case Url.fromString tab.url of
        Just url ->
            if C.checkIfIntercept model url then
                setRedirect
                    (encodeRedirect tab.id <|
                        String.concat
                            [ "/intercept.html?next="
                            , url |> Url.toString |> Url.percentEncode
                            ]
                    )

            else
                Cmd.none

        Nothing ->
            consoleLog ("Could not parse URL from string: " ++ tab.url)


encodeRedirect : Int -> String -> Json.Encode.Value
encodeRedirect tabId url =
    Json.Encode.object
        [ ( "tabId", Json.Encode.int tabId )
        , ( "url", Json.Encode.string url )
        ]


decodeUrlChange : Json.Decode.Value -> Result Json.Decode.Error C.Tab
decodeUrlChange urlChangeJson =
    Json.Decode.decodeValue tabDecoder urlChangeJson


decodeTabs : Json.Decode.Value -> Result Json.Decode.Error (List C.Tab)
decodeTabs tabsJson =
    Json.Decode.decodeValue (Json.Decode.list tabDecoder) tabsJson


tabDecoder : Json.Decode.Decoder C.Tab
tabDecoder =
    Json.Decode.map2 C.Tab
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
