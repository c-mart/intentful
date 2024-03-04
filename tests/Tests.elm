module Tests exposing (messageRoundTrip, modelRoundTrip, registeredDomains)

import Common as C
import Expect
import Json.Decode
import Set
import Test exposing (..)
import Time


registeredDomainTestData =
    [ ( "www.visitarizona.com", "visitarizona.com" )
    , ( "foobar.bbc.co.uk", "bbc.co.uk" )
    , ( "cmart.blog", "cmart.blog" )
    , ( "package.elm-lang.org", "elm-lang.org" )
    ]


registeredDomains : Test
registeredDomains =
    let
        toTest hostname registeredDomain =
            test
                ("Hostname " ++ hostname ++ " has registered domain " ++ registeredDomain)
                (\_ ->
                    Expect.equal (C.hostnameToRegisteredDomain hostname) registeredDomain
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
    C.Model [ { id = 123, url = "https://lobste.rs" } ] (Set.fromList [ "weather.gov" ]) Set.empty [ testException ]


testException : C.Exception
testException =
    C.Exception "facebook.com" (Time.millisToPosix 1704223664)


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
