module Main exposing (main)

import Browser exposing (..)
import Browser.Navigation as Nav
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Login
import Me
import NotFound
import Route exposing (Route)
import Session
import Sessions
import SignUp
import Url exposing (Url)
import UserSession
import Viewer exposing (Viewer)


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | LoginMsg Login.Msg
    | SignUpMsg SignUp.Msg
    | HomeMsg Home.Msg
    | NotFoundMsg NotFound.Msg
    | MeMsg Me.Msg
    | SessionsMsg Sessions.Msg
    | SessionMsg Session.Msg


type Model
    = LoginModel Login.Model
    | SignUpModel SignUp.Model
    | HomeModel Home.Model
    | NotFoundModel NotFound.Model
    | MeModel Me.Model
    | SessionsModel Sessions.Model
    | SessionModel Session.Model


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case flags of
        Just token ->
            changeModel
                (Route.toRoute url)
                { key = key, token = Just token }

        Nothing ->
            ( Tuple.first
                (changeModel Route.Login
                    { key = key
                    , token = Nothing
                    }
                )
            , Nav.replaceUrl key "/login"
            )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        LoginModel subModel ->
            Login.view subModel
                |> viewTransformer LoginMsg

        SignUpModel subModel ->
            SignUp.view subModel
                |> viewTransformer SignUpMsg

        HomeModel subModel ->
            Home.view subModel
                |> viewTransformer HomeMsg

        NotFoundModel subModel ->
            NotFound.view subModel
                |> viewTransformer NotFoundMsg

        MeModel subModel ->
            Me.view subModel
                |> viewTransformer MeMsg

        SessionsModel subModel ->
            Sessions.view subModel
                |> viewTransformer SessionsMsg

        SessionModel subModel ->
            Session.view subModel
                |> viewTransformer SessionMsg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            changeModel (Route.toRoute url) (getSession model)

        ( LinkClicked urlRequest, _ ) ->
            case (getSession model).token of
                Just token ->
                    case urlRequest of
                        Internal url ->
                            ( model
                            , Nav.pushUrl (getSession model).key (Url.toString url)
                            )

                        External url ->
                            ( model
                            , Nav.load url
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ( LoginMsg subMsg, LoginModel subModel ) ->
            Login.update subMsg subModel
                |> transform LoginMsg LoginModel

        ( SignUpMsg subMsg, SignUpModel subModel ) ->
            SignUp.update subMsg subModel
                |> transform SignUpMsg SignUpModel

        ( HomeMsg subMsg, HomeModel subModel ) ->
            Home.update subMsg subModel
                |> transform HomeMsg HomeModel

        ( NotFoundMsg subMsg, NotFoundModel subModel ) ->
            NotFound.update subMsg subModel
                |> transform NotFoundMsg NotFoundModel

        ( MeMsg subMsg, MeModel subModel ) ->
            Me.update subMsg subModel
                |> transform MeMsg MeModel

        ( SessionsMsg subMsg, SessionsModel subModel ) ->
            Sessions.update subMsg subModel
                |> transform SessionsMsg SessionsModel

        ( SessionMsg subMsg, SessionModel subModel ) ->
            Session.update subMsg subModel
                |> transform SessionMsg SessionModel

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



-- HELPERS


changeModel : Route -> UserSession.Session -> ( Model, Cmd Msg )
changeModel route session =
    case route of
        Route.Home ->
            Home.init session
                |> transform HomeMsg HomeModel

        Route.Login ->
            Login.init session
                |> transform LoginMsg LoginModel

        Route.SignUp ->
            SignUp.init session
                |> transform SignUpMsg SignUpModel

        Route.NotFound ->
            NotFound.init session
                |> transform NotFoundMsg NotFoundModel

        Route.Me ->
            Me.init session
                |> transform MeMsg MeModel

        Route.Sessions ->
            Sessions.init session
                |> transform SessionsMsg SessionsModel

        Route.Session id ->
            Session.init session
                |> transform SessionMsg SessionModel


transform : (subMsg -> Msg) -> (subModel -> Model) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
transform toMsg toModel ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


getSession : Model -> UserSession.Session
getSession model =
    case model of
        LoginModel loginModel ->
            loginModel.userSession

        SignUpModel signUpModel ->
            signUpModel.userSession

        HomeModel homeModel ->
            homeModel.userSession

        NotFoundModel notFoundModel ->
            notFoundModel.userSession

        MeModel meModel ->
            meModel.userSession

        SessionsModel sessionsModel ->
            sessionsModel.userSession

        SessionModel sessionModel ->
            sessionModel.userSession


viewTransformer : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewTransformer toMsg subMsg =
    let
        { title, body } =
            subMsg
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoginModel loginModel ->
            Sub.map LoginMsg (Login.subscriptions loginModel)

        SignUpModel signUpModel ->
            Sub.map SignUpMsg (SignUp.subscriptions signUpModel)

        HomeModel homeModel ->
            Sub.map HomeMsg (Home.subscriptions homeModel)

        NotFoundModel notFoundModel ->
            Sub.map NotFoundMsg (NotFound.subscriptions notFoundModel)

        MeModel meModel ->
            Sub.map MeMsg (Me.subscriptions meModel)

        SessionsModel sessionsModel ->
            Sub.map SessionsMsg (Sessions.subscriptions sessionsModel)

        SessionModel sessionModel ->
            Sub.map SessionMsg (Session.subscriptions sessionModel)
