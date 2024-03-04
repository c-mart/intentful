module Tests exposing (messageRoundTrip, modelRoundTrip, registeredDomains)

import Common as C
import Expect
import Json.Decode
import Set
import Test exposing (..)
import Time


registeredDomainTestData =
    [ ( "www.visitarizona.com", C.RegisteredDomain "visitarizona.com" )
    , ( "foobar.bbc.co.uk", C.RegisteredDomain "bbc.co.uk" )
    , ( "cmart.blog", C.RegisteredDomain "cmart.blog" )
    , ( "package.elm-lang.org", C.RegisteredDomain "elm-lang.org" )
    ]


registeredDomains : Test
registeredDomains =
    let
        toTest hostname (C.RegisteredDomain rDom) =
            test
                ("Hostname " ++ hostname ++ " has registered domain " ++ rDom)
                (\_ ->
                    Expect.equal (hostname |> C.hostnameToRegisteredDomain |> C.unwrapRegisteredDomain) rDom
                )
    in
    describe "Registered domain"
        (List.map
            (\( hostname, registeredDomain ) ->
                toTest hostname registeredDomain
            )
            registeredDomainTestData
        )


testModel : C.Model
testModel =
    C.Model [ { id = 123, url = "https://lobste.rs" } ] [ "weather.gov" ] [] [ testException ]


testException : C.Exception
testException =
    C.Exception (C.Hostname "facebook.com") (Time.millisToPosix 1704223664)


modelRoundTrip : Test
modelRoundTrip =
    test "Round trip model" <|
        \_ ->
            Expect.equal
                (Ok testModel)
                (testModel
                    |> C.encodeModel
                    |> Json.Decode.decodeValue C.modelDecoder
                )


messageRoundTrip : Test
messageRoundTrip =
    let
        messagesFromInterceptPage =
            [ { m = C.RequestModel
              , d = "request model"
              }
            , { m = C.NewException testException
              , d = "new exception"
              }
            ]

        messagesFromBGScript =
            [ { m = C.SendModel testModel
              , d = "send model"
              }
            ]
    in
    describe "Round trip messages" <|
        List.concat
            [ List.map
                (\message ->
                    test message.d <|
                        \_ ->
                            Expect.equal (Ok message.m)
                                (message.m
                                    |> C.encodeMessageToBackgroundScript
                                    |> Json.Decode.decodeValue C.messageToBackgroundScriptDecoder
                                )
                )
                messagesFromInterceptPage
            , List.map
                (\message ->
                    test message.d <|
                        \_ ->
                            Expect.equal (Ok message.m)
                                (message.m
                                    |> C.encodeMessageFromBackgroundScript
                                    |> Json.Decode.decodeValue C.messageFromBackgroundScriptDecoder
                                )
                )
                messagesFromBGScript
            ]
