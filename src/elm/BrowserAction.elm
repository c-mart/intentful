port module BrowserAction exposing (..)

import Background exposing (Msg(..))
import Browser
import Common as C
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as HtmlE
import Json.Decode
import Json.Encode
import Url
import Url.Parser exposing ((<?>))



-- Primary types


type AppValidity
    = AppValid Model
    | AppInvalid String


type alias Model =
    { common : C.Model
    , currentTabUrl : Url.Url
    }


type Msg
    = ReceiveMessage Json.Encode.Value
    | ReceiveCurrentTabUrl String
    | GotSetSiteStatus C.SiteStatus



-- Helper types
-- Ports


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port receiveCurrentTabUrl : (String -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg



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
        commonModel =
            Json.Decode.decodeValue (Json.Decode.field "common-model" C.modelDecoder) flags

        currentTabUrl =
            Json.Decode.decodeValue
                (Json.Decode.field "currentTabUrl" Json.Decode.string
                    |> Json.Decode.map Url.fromString
                )
                flags
    in
    case ( commonModel, currentTabUrl ) of
        ( Ok m, Ok (Just u) ) ->
            ( AppValid <| Model m u, Cmd.none )

        ( Err decodeErr, _ ) ->
            ( AppInvalid (Json.Decode.errorToString decodeErr), Cmd.none )

        ( _, Err decodeErr ) ->
            ( AppInvalid (Json.Decode.errorToString decodeErr), Cmd.none )

        ( _, Ok Nothing ) ->
            ( AppInvalid "could not parse current URL", Cmd.none )


update : Msg -> AppValidity -> ( AppValidity, Cmd a )
update msg validity =
    case validity of
        AppValid model ->
            updateValid msg model
                |> Tuple.mapFirst AppValid

        AppInvalid _ ->
            ( validity, Cmd.none )


updateValid : Msg -> Model -> ( Model, Cmd a )
updateValid msg model =
    case msg of
        ReceiveMessage value ->
            case Json.Decode.decodeValue C.messageFromBackgroundScriptDecoder value of
                Ok message ->
                    case message of
                        C.SendModel commonModel ->
                            ( { model | common = commonModel }
                            , Cmd.none
                            )

                Err e ->
                    -- TODO something with this error
                    ( model, Cmd.none )

        ReceiveCurrentTabUrl urlStr ->
            case Url.fromString urlStr of
                Just url ->
                    ( { model | currentTabUrl = url }, Cmd.none )

                Nothing ->
                    -- TODO maybe model invalid URL, or debug log, or something
                    ( model, Cmd.none )

        GotSetSiteStatus status ->
            let
                -- For now, always set registered domain. Later, we'll allow user to choose scope of site.
                rDom =
                    model.currentTabUrl.host
                        |> C.Hostname
                        |> C.hostnameToRegisteredDomain

                hostname =
                    rDom |> C.unwrapRegisteredDomain |> C.Hostname

                message =
                    C.encodeMessageToBackgroundScript (C.SetSiteStatus hostname status)
            in
            ( model, sendMessage message )


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


viewValid : Model -> Html Msg
viewValid model =
    Html.div []
        [ if model.common.mode == C.TestMode then
            C.testModeBanner

          else
            Html.text ""
        , Html.text
            ("Current site is "
                ++ (model.currentTabUrl.host |> C.Hostname |> C.hostnameToRegisteredDomain |> C.unwrapRegisteredDomain)
            )
        , Html.text
            ("This site is "
                ++ Debug.toString (C.checkSiteStatus model.common model.currentTabUrl)
            )
        , renderSetStatusButtons model
        ]


renderSetStatusButtons : Model -> Html Msg
renderSetStatusButtons model =
    let
        options =
            case C.checkSiteStatus model.common model.currentTabUrl of
                C.Unknown ->
                    [ ( C.Safe, "Safe" )
                    , ( C.Unsafe, "Unsafe" )
                    ]

                C.Safe ->
                    [ ( C.Unsafe, "Unsafe" )
                    ]

                C.Unsafe ->
                    -- TODO make this harder to select, it decreases safety
                    [ ( C.Safe, "Safe" )
                    ]

        renderButton option =
            Html.button
                [ HtmlE.onClick (GotSetSiteStatus (Tuple.first option)) ]
                [ Html.text (Tuple.second option) ]
    in
    Html.div
        []
        (List.map renderButton options)


subs : Sub Msg
subs =
    Sub.batch
        [ receiveMessage ReceiveMessage
        , receiveCurrentTabUrl ReceiveCurrentTabUrl
        ]



-- Helper functions
