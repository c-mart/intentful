module Tests exposing (messageRoundTrip, modelRoundTrip)

import Common as C
import Expect
import Json.Decode
import Test exposing (..)
import Time


testModel : C.Model
testModel =
    C.Model [ "reddit.com" ] [ testException ]


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
                                    |> C.encodeMessageFromInterceptPage
                                    |> Json.Decode.decodeValue C.decodeMessageFromInterceptPage
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
                                    |> Json.Decode.decodeValue C.decodeMessageFromBackgroundScript
                                )
                )
                messagesFromBGScript
            ]