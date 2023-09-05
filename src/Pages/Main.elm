module Pages.Main exposing (Model, Msg, page)

import Html
import Page exposing (Page)
import RenderMain.Model exposing (init)
import RenderMain.Msg exposing (Msg(..))
import RenderMain.Subscription exposing (subscriptions)
import RenderMain.Update exposing (update)
import RenderMain.View exposing (view)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , view = generateHtml "Schedules" view
        , subscriptions = subscriptions
        }



-- |> Page.withOnUrlChanged (UrlChange shared)


generateHtml : String -> (Model -> Html.Html Msg) -> Model -> View Msg
generateHtml title view model =
    { title = title
    , body = [ view model ]
    }


type alias Msg =
    RenderMain.Msg.Msg


type alias Model =
    RenderMain.Model.Model
