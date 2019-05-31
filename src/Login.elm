module Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode exposing (encode, object, string)
import Port exposing (onStoreChange, storeCache)
import Url
import UserSession



-- MODEL


type HttpResponse
    = Dormant
    | Loading
    | Failure


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


tokenDecoder : Decoder String
tokenDecoder =
    field "token" Json.Decode.string


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
                , expect = Http.expectJson LoginResponseHandler tokenDecoder
                }
            )

        LoginResponseHandler result ->
            case result of
                Ok token ->
                    ( model, storeCache (Just token) )

                Err _ ->
                    ( { model | authResponse = Failure }, Cmd.none )

        GotSession token ->
            ( { model | userSession = { token = Just token, key = model.userSession.key } }, Nav.replaceUrl model.userSession.key "/" )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Login"
    , body =
        [ div []
            [ input [ type_ "text", placeholder "example@gmail.com", value model.email, onInput EmailUpdater ] []
            , input [ type_ "password", value model.password, onInput PasswordUpdater ] []
            , button [ onClick (AuthenticateUser model.email model.password) ] [ text "submit" ]
            , renderSuccessMessage model.authResponse
            ]
        ]
    }


renderSuccessMessage : HttpResponse -> Html Msg
renderSuccessMessage authResponse =
    case authResponse of
        Failure ->
            div [] [ text "Failure" ]

        Loading ->
            div [] [ text "Loading" ]

        Dormant ->
            div [] [ text "" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    UserSession.transformer GotSession
