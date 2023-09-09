module Pages.Home_ exposing (Model, Msg, page)

import Decoders exposing (tokenParser)
import Effect exposing (Effect)
import Encoders exposing (login)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared.backendUrl
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { username : String
    , password : String
    , error : String
    , loginUrl : String
    }


init : String -> () -> ( Model, Effect Msg )
init loginUrl () =
    ( { username = "", password = "", error = "", loginUrl = loginUrl }, Effect.none )



-- UPDATE


type Msg
    = GotToken (Result Http.Error String)
    | GotError String
    | SendLoginRequest String String
    | SetUsername String
    | SetPassword String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotToken result ->
            case result of
                Ok token ->
                    ( model, Effect.loadToken token )

                Err (Http.BadStatus 401) ->
                    ( { model | error = "nome de utilizador ou senha inválidos" }, Effect.none )

                Err err ->
                    ( { model | error = Decoders.errorToString err }, Effect.none )

        GotError str ->
            ( { model | error = str }, Effect.none )

        SendLoginRequest username password ->
            ( model, Effect.sendCmd (logIn username password model.loginUrl) )

        SetUsername username ->
            ( { model | username = username }, Effect.none )

        SetPassword password ->
            ( { model | password = password }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        [ div
            [ id "form" ]
            [ h2 [ class "text-center" ] [ text "Log In" ]
            , div [ class "showError" ]
                [ div [ class "alert alert-danger" ] [ text model.error ]
                ]
            , div [ class "form-group row" ]
                [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                    ]
                ]
            , div [ class "form-group row" ]
                [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                    ]
                ]
            , div [ class "text-center" ]
                [ button [ class "btn btn-link", onClick (SendLoginRequest model.username model.password) ] [ text "Login" ]
                ]
            ]
        ]
    }


logIn : String -> String -> String -> Cmd Msg
logIn username password url =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url ++ "login"
        , body = Http.jsonBody (login username password)
        , expect = Http.expectJson GotToken tokenParser
        , timeout = Nothing
        , tracker = Nothing
        }
