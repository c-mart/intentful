port module Background exposing (..)

import Common as C
import Debug
import Json.Decode
import Json.Encode
import Time
import Url



-- Primary types


type Msg
    = GotUrlChange Json.Encode.Value
    | GotMessage Json.Encode.Value
    | GotCurrentTime Time.Posix
    | GotTabs Json.Encode.Value


type alias Tab =
    { id : Int
    , url : String
    }



-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port getUrlChange : (Json.Encode.Value -> msg) -> Sub msg


port setRedirect : Json.Encode.Value -> Cmd msg


port requestTabs : () -> Cmd msg


port receiveTabs : (Json.Encode.Value -> msg) -> Sub msg


port setStorage : Json.Encode.Value -> Cmd msg


port consoleLog : String -> Cmd msg



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
                    { domainsToRedirect =
                        [ "reddit.com"
                        ]
                    , exceptions = []
                    }
    in
    ( model
    , Cmd.batch [ requestTabs (), setStorage (C.encodeModel model) ]
    )


update : Msg -> C.Model -> ( C.Model, Cmd msg )
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
        , Time.every 5000 GotCurrentTime
        ]



-- Helper functions


innerUpdate : Msg -> C.Model -> ( C.Model, Cmd msg )
innerUpdate msg model =
    case msg of
        GotMessage value ->
            case Json.Decode.decodeValue C.messageFromInterceptPageDecoder value of
                Ok message ->
                    case message of
                        C.RequestModel ->
                            ( model, sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel model) )

                        C.NewException exception ->
                            ( { model | exceptions = exception :: model.exceptions }, Cmd.none )

                Err e ->
                    ( model
                    , logJsonError "Could not decode message" e
                    )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok tab ->
                    ( model, processTab model tab )

                Err e ->
                    ( model
                    , logJsonError "Could not decode URL change" e
                    )

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
                    ( model, Cmd.batch <| List.map (processTab model) tabs )

                Err e ->
                    ( model
                    , logJsonError "Could not decode tabs" e
                    )


logJsonError : String -> Json.Decode.Error -> Cmd msg
logJsonError description e =
    let
        eStr =
            description ++ ": " ++ Json.Decode.errorToString e
    in
    consoleLog eStr


processTab : C.Model -> Tab -> Cmd msg
processTab model tab =
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


decodeUrlChange : Json.Decode.Value -> Result Json.Decode.Error Tab
decodeUrlChange urlChangeJson =
    Json.Decode.decodeValue tabDecoder urlChangeJson


decodeTabs : Json.Decode.Value -> Result Json.Decode.Error (List Tab)
decodeTabs tabsJson =
    Json.Decode.decodeValue (Json.Decode.list tabDecoder) tabsJson


tabDecoder : Json.Decode.Decoder Tab
tabDecoder =
    Json.Decode.map2 Tab
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
