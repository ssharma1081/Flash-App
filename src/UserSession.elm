module UserSession exposing (Session, transformer)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode exposing (encode, object, string)
import Port exposing (onStoreChange)
import Url



-- MODEL


type alias Session =
    { token : Maybe String
    , key : Nav.Key
    }


transformer : (String -> msg) -> Sub msg
transformer toMsg =
    onStoreChange (\value -> toMsg value)
