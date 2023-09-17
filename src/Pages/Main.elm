module Pages.Main exposing (Model, Msg, page)

import Html
import Main.Model exposing (init)
import Main.Msg exposing (Msg(..))
import Main.Subscription exposing (subscriptions)
import Main.Update exposing (update)
import Main.View exposing (view)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , view = generateHtml "Horarios" view
        , subscriptions = subscriptions
        }


generateHtml : String -> (Model -> Html.Html Msg) -> Model -> View Msg
generateHtml title view model =
    { title = title
    , body = [ view model ]
    }


type alias Msg =
    Main.Msg.Msg


type alias Model =
    Main.Model.Model
