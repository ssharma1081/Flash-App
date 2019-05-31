module Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode exposing (encode, object, string)
import Url
import UserSession
import Viewer exposing (Viewer, viewerDecoder)



-- MODEL


type ViewerResponse
    = Loading
    | Failure
    | Success


type alias Model =
    { viewer : Maybe Viewer
    , userSession : UserSession.Session
    , viewerResponse : ViewerResponse
    }


init : UserSession.Session -> ( Model, Cmd Msg )
init currentSession =
    case currentSession.token of
        Just token ->
            ( { viewer = Nothing
              , viewerResponse = Loading
              , userSession = currentSession
              }
            , Http.post
                { url = "http://localhost:3000/user/me"
                , expect = Http.expectJson MeResponseHandler viewerDecoder
                , body = Http.jsonBody (object [ ( "token", string token ) ])
                }
            )

        Nothing ->
            ( { viewer = Nothing
              , viewerResponse = Loading
              , userSession = currentSession
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = GotSession String
    | MeResponseHandler (Result Http.Error Viewer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession token ->
            ( { model | userSession = { token = Just token, key = model.userSession.key } }, Cmd.none )

        MeResponseHandler result ->
            case result of
                Ok currentViewer ->
                    ( { model | viewer = Just currentViewer, viewerResponse = Success }, Cmd.none )

                Err _ ->
                    ( { model | viewerResponse = Failure }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Home"
    , body =
        case model.viewerResponse of
            Loading ->
                [ div [] [ text "Loading..." ] ]

            Success ->
                case model.viewer of
                    Just viewer ->
                        [ div [] [ text viewer.name ]
                        , div [] [ text viewer.email ]
                        , li [] [ a [ href "/me/sessions" ] [ text "sessions" ] ]
                        , li [] [ a [ href "/me" ] [ text "me" ] ]
                        , li [] [ a [ href "/login" ] [ text "login" ] ]
                        , li [] [ a [ href "/sign-up" ] [ text "sign-up" ] ]
                        , li [] [ a [ href "/" ] [ text "home" ] ]
                        , li [] [ a [ href "/me/session" ] [ text "session" ] ]
                        ]

                    Nothing ->
                        [ div [] [ text "Hmm...this is embarassing." ] ]

            Failure ->
                [ div [] [ text "Something's not right." ] ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    UserSession.transformer GotSession
