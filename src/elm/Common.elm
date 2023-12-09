module Common exposing (..)

import Json.Decode
import Json.Encode


type alias Model =
    { domainsToRedirect : List String
    }


modelDecoder =
    Json.Decode.map Model
        (Json.Decode.field "domainsToRedirect"
            (Json.Decode.list Json.Decode.string)
        )


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "domainsToRedirect"
          , Json.Encode.list Json.Encode.string model.domainsToRedirect
          )
        ]
