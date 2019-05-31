module Route exposing (Route(..), toRoute)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = Login
    | SignUp
    | Home
    | NotFound
    | Me
    | Sessions
    | Session Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map SignUp (s "sign-up")
        , map Me (s "me")
        , map Sessions (s "me" </> s "sessions")
        , map Session (s "me" </> s "session" </> int)
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse parser url)
