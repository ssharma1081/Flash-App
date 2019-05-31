module Session exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode exposing (encode, object, string)
import Url
import UserSession



-- MODEL


type HttpResponse
    = Dormant
    | Loading
    | Failure
    | Response String


type alias Model =
    { email : String
    , password : String
    , authResponse : HttpResponse
    , numOfAttempts : Int
    , userSession : UserSession.Session
    }


init : UserSession.Session -> ( Model, Cmd Msg )
init currentSession =
    ( { email = ""
      , password = ""
      , authResponse = Dormant
      , numOfAttempts = 0
      , userSession = currentSession
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EmailUpdater String
    | PasswordUpdater String
    | AuthenticateUser String String
    | LoginResponseHandler (Result Http.Error String)
    | GotSession String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailUpdater newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        PasswordUpdater newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        AuthenticateUser email password ->
            ( { model | authResponse = Loading, numOfAttempts = model.numOfAttempts + 1 }
            , Http.post
                { body = Http.jsonBody (object [ ( "username", string model.email ), ( "password", string model.password ) ])
                , url = "http://localhost:3000/sign-in"
                , expect = Http.expectString LoginResponseHandler
                }
            )

        LoginResponseHandler result ->
            case result of
                Ok response ->
                    ( { model | authResponse = Response response }, Cmd.none )

                Err _ ->
                    ( { model | authResponse = Failure }, Cmd.none )

        GotSession token ->
            ( { model | userSession = { token = Just token, key = model.userSession.key } }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sessions"
    , body =
        [ li [] [ text "Singular Session" ] ]
    }


renderSuccessMessage : HttpResponse -> Html Msg
renderSuccessMessage authResponse =
    case authResponse of
        Response response ->
            if response == "OK" then
                div [] [ text "Success" ]

            else
                div [] [ text response ]

        Failure ->
            div [] [ text "Failure" ]

        Loading ->
            div [] [ text "Loading" ]

        Dormant ->
            div [] [ text "" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    UserSession.transformer GotSession
