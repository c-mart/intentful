port module Intercept exposing (..)

import Browser
import Common as C
import Debug
import Html exposing (Html)
import Json.Decode
import Json.Encode
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
        href =
            Json.Decode.decodeValue (Json.Decode.field "href" Json.Decode.string) flags
                |> Result.mapError Json.Decode.errorToString

        urlStrToNextUrl uS =
            let
                hrefSchemeSwap =
                    -- https://github.com/elm/url/issues/10
                    uS
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
            Result.andThen urlStrToNextUrl href
    in
    ( Model Waiting nextUrl, Cmd.none )


type alias Model =
    { common : CommonModel
    , nextUrl : Result String String
    }


type CommonModel
    = Valid C.Model
    | Invalid String
    | Waiting


decodeCommonModel : Json.Decode.Value -> CommonModel
decodeCommonModel flags =
    case Json.Decode.decodeValue C.modelDecoder flags of
        Ok commonModel ->
            Valid commonModel

        Err e ->
            Invalid (Json.Decode.errorToString e)


type Msg
    = GotCommonModel Json.Encode.Value


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        GotCommonModel value ->
            ( { model | common = decodeCommonModel value }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        text =
            case model.common of
                Waiting ->
                    "Waiting for common model"

                Valid m ->
                    "Got model: " ++ Debug.toString m

                Invalid s ->
                    "Invalid model: " ++ s
    in
    Html.text (text ++ " and you were going to " ++ Debug.toString model.nextUrl)


subs : Sub Msg
subs =
    receiveCommonModel GotCommonModel


port receiveCommonModel : (Json.Encode.Value -> msg) -> Sub msg