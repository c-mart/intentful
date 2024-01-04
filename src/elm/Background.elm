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



-- Primary functions


main : Program Json.Decode.Value C.Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subs
        }


init flags =
    ( { domainsToRedirect =
            [ "reddit.com"
            ]
      , exceptions = []
      }
    , requestTabs ()
    )


update : Msg -> C.Model -> ( C.Model, Cmd msg )
update msg model =
    let
        ( newModel, cmd ) =
            innerUpdate msg model
    in
    ( newModel, Cmd.batch [ cmd, sendMessage <| C.encodeMessageFromBackgroundScript (C.SendModel newModel) ] )


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
                    -- TODO something with this error
                    ( model, Cmd.none )

        GotUrlChange urlChangeJson ->
            case decodeUrlChange urlChangeJson of
                Ok tab ->
                    ( model, processTab model tab )

                Err _ ->
                    -- TODO complain?
                    ( model, Cmd.none )

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
                    -- TODO complain?
                    ( model, Cmd.none )


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
            -- TODO complain?
            Cmd.none


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
