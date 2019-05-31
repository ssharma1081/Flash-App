port module Port exposing (onStoreChange, storeCache)

import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)


port storeCache : Maybe String -> Cmd msg


port onStoreChange : (String -> msg) -> Sub msg
