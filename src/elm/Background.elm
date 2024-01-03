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



-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port getUrlChange : (Json.Encode.Value -> msg) -> Sub msg


port setRedirect : Json.Encode.Value -> Cmd msg



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
    , Cmd.none
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
                Ok ( tabId, urlStr ) ->
                    case Url.fromString urlStr of
                        Just url ->
                            if C.checkIfIntercept model url then
                                ( model
                                , setRedirect
                                    (encodeRedirect tabId <|
                                        String.concat
                                            [ "/intercept.html?next="
                                            , url |> Url.toString |> Url.percentEncode
                                            ]
                                    )
                                )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            -- TODO complain?
                            ( model, Cmd.none )

                Err _ ->
                    -- TODO complain?
                    ( model, Cmd.none )

        GotCurrentTime time ->
            let
                -- TODO when removing exception, redirect any tabs on that site to the intercept page.
                keepException : C.Exception -> Bool
                keepException e =
                    Time.posixToMillis e.endTime > Time.posixToMillis time

                newExceptions =
                    List.filter keepException model.exceptions
            in
            ( { model | exceptions = newExceptions }, Cmd.none )


encodeRedirect : Int -> String -> Json.Encode.Value
encodeRedirect tabId url =
    Json.Encode.object
        [ ( "tabId", Json.Encode.int tabId )
        , ( "url", Json.Encode.string url )
        ]


decodeUrlChange : Json.Decode.Value -> Result Json.Decode.Error ( Int, String )
decodeUrlChange urlChangeJson =
    let
        urlChangeDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "tabId" Json.Decode.int)
                (Json.Decode.field "url" Json.Decode.string)
    in
    Json.Decode.decodeValue urlChangeDecoder urlChangeJson
