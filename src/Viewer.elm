module Viewer exposing (Viewer, viewerDecoder)

import Json.Decode exposing (Decoder, field, map2)


type alias Viewer =
    { email : String
    , name : String
    }


viewerDecoder : Decoder Viewer
viewerDecoder =
    map2 Viewer
        (field "name" Json.Decode.string)
        (field "email" Json.Decode.string)
