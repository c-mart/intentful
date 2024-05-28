port module Background exposing (..)

import Common as C exposing (Mode(..), RegisteredDomain)
import Debug
import Html.Styled exposing (a)
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
    | GotSetTestMode Int



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


port setTestMode : (Int -> msg) -> Sub msg



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
                    , unsafeSites =
                        List.map C.Hostname C.unsafeSitesForDevTesting
                    , safeSites =
                        []
                    , exceptions = []
                    , mode = C.NormalMode
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
        , setTestMode GotSetTestMode
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

                        C.SetSiteStatus hostname status ->
                            setSiteStatus model hostname status

                Err e ->
                    ( model
                    , logJsonError "Could not decode message" e
                    )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok navEvent ->
                    if navEvent.frameId == 0 then
                        let
                            tab =
                                C.Tab navEvent.tabId navEvent.url

                            newModel =
                                modelUpdateTab model tab
                        in
                        ( newModel, processTab newModel tab )

                    else
                        -- Navigation happened within a nested iframe, so we don't care about it right now
                        ( model, Cmd.none )

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

        GotSetTestMode epoch ->
            let
                expireTime =
                    -- 2 hours from now
                    epoch + (2 * 3600 * 1000) |> Time.millisToPosix

                newModel =
                    { model | mode = TestMode expireTime }
            in
            ( newModel, Cmd.none )


setSiteStatus : C.Model -> C.Hostname -> C.SiteStatus -> ( C.Model, Cmd Msg )
setSiteStatus model hostname status =
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
                | unsafeSites =
                    List.filter (\d -> d /= hostname) model.unsafeSites
                , safeSites =
                    List.filter (\d -> d /= hostname) model.safeSites
              }
            , Cmd.none
            )

        C.Safe ->
            ( { model
                | unsafeSites =
                    List.filter (\d -> d /= hostname) model.unsafeSites
                , safeSites =
                    insert hostname model.safeSites
              }
            , Cmd.none
            )

        C.Unsafe ->
            let
                newModel =
                    { model
                        | unsafeSites =
                            insert hostname model.unsafeSites
                        , safeSites =
                            List.filter (\d -> d /= hostname) model.safeSites
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


decodeUrlChange : Json.Decode.Value -> Result Json.Decode.Error C.NavigationEvent
decodeUrlChange urlChangeJson =
    Json.Decode.decodeValue navigationEventDecoder urlChangeJson


decodeTabs : Json.Decode.Value -> Result Json.Decode.Error (List C.Tab)
decodeTabs tabsJson =
    Json.Decode.decodeValue (Json.Decode.list tabDecoder) tabsJson


tabDecoder : Json.Decode.Decoder C.Tab
tabDecoder =
    Json.Decode.map2 C.Tab
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)


navigationEventDecoder : Json.Decode.Decoder C.NavigationEvent
navigationEventDecoder =
    Json.Decode.map3 C.NavigationEvent
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "frameId" Json.Decode.int)
