module Pages.EditLect.Id_ exposing (Model, Msg, page)

import Decoders exposing (IsHidden)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Encoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Maybe.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import ScheduleObjects.Data exposing (Data, Token)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID)
import ScheduleObjects.Restriction as Restriction exposing (Restriction, RestrictionID, categoryComparator)
import ScheduleObjects.WeekTimeConverters exposing (convertWeekDay, convertWeekTimeHourAndMinute, weekTimeComparator)
import Shared
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route.params.id
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type Model
    = Model ( LecturerID, Lecturer ) (Dict RestrictionID Restriction) String String Bool String Bool


init : Data -> String -> () -> ( Model, Effect Msg )
init data lectIDParam () =
    let
        lectID =
            String.toInt lectIDParam
                |> Maybe.Extra.withDefaultLazy (\() -> -1)

        ( lect, isHidden ) =
            case Dict.get lectID data.lecturers of
                Just l ->
                    ( l, False )

                _ ->
                    ( Dict.get lectID data.hiddenLecturers
                        |> Maybe.Extra.withDefaultLazy (\() -> Lecturer "" "" "")
                    , True
                    )

        -- |> Maybe.Extra.withDefaultLazy (\() -> Lecturer "" "" "")
        -- restrictions =
    in
    ( Model ( lectID, lect ) (Dict.filter (\_ restriction -> restriction.lect == lectID) data.restrictions) data.backendUrl data.token False "" isHidden, Effect.none )



-- UPDATE


type Msg
    = AbbrChange String
    | NameChange String
    | OfficeChange String
    | UpdateLectRequest
    | UpdateLectResult (Result Http.Error ( LecturerID, ( Lecturer, IsHidden ) ))
    | DeleteLectRequest
    | DeleteLectResult (Result Http.Error ())
    | GoToAddRestrictionPage
    | GoToEditRestrictionPage RestrictionID
    | VisibilityChange Bool
    | Return


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden) =
    case msg of
        AbbrChange str ->
            ( Model ( lectID, { lect | abbr = str } ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.none )

        NameChange str ->
            ( Model ( lectID, { lect | name = str } ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.none )

        OfficeChange str ->
            ( Model ( lectID, { lect | office = str } ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.none )

        UpdateLectRequest ->
            ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.sendCmd <| updateLect ( lectID, lect ) isHidden backendUrl token )

        UpdateLectResult result ->
            case result of
                Ok ( id, updatedLect ) ->
                    let
                        route =
                            { path = Route.Path.Main
                            , query = Dict.empty
                            , hash = Nothing
                            }
                    in
                    ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.updateLect ( id, updatedLect ) (Just route) )

                Err err ->
                    ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation (Decoders.errorToString err) isHidden, Effect.none )

        DeleteLectRequest ->
            if deleteConfirmation then
                ( Model ( lectID, lect ) restrictions backendUrl token False errorMsg isHidden, Effect.sendCmd <| deleteLect lectID backendUrl token )

            else
                ( Model ( lectID, lect ) restrictions backendUrl token True errorMsg isHidden, Effect.none )

        DeleteLectResult result ->
            case result of
                Ok _ ->
                    ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.deleteLect lectID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation "Erro no servidor. Verifique se ainda existem cadeiras (incluindo cadeiras escondidas) associadas a este docente." isHidden, Effect.none )

                Err err ->
                    ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation (Decoders.errorToString err) isHidden, Effect.none )

        GoToEditRestrictionPage id ->
            ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.pushRoute { path = Route.Path.EditRestriction_Id_ { id = String.fromInt id }, query = Dict.empty, hash = Nothing } )

        GoToAddRestrictionPage ->
            ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.pushRoute { path = Route.Path.AddRestriction, query = Dict.singleton "lectID" (String.fromInt lectID), hash = Nothing } )

        VisibilityChange newVisibility ->
            ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg newVisibility, Effect.none )

        Return ->
            ( Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view (Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden) =
    let
        orderedRestrictions =
            restrictions
                |> Dict.toList
                |> List.sortWith
                    (\( _, r1 ) ( _, r2 ) ->
                        if r1.category == r2.category then
                            weekTimeComparator r1.start_time r2.start_time

                        else
                            categoryComparator r1.category r2.category
                    )
    in
    { title = "Editar Docente"
    , body =
        [ input [ class "input-box", style "width" "100%", value lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Do Docente" ] []
        , input [ class "input-box", style "width" "100%", value lect.office, onInput OfficeChange, Html.Attributes.placeholder "Escritorio" ] []
        , renderRestrictions orderedRestrictions
        , div [] [ input [ type_ "checkbox", checked isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Docente" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteLectRequest ]
            [ text
                (if deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text errorMsg ]
        ]
    }


renderRestrictions : List ( RestrictionID, Restriction ) -> Html Msg
renderRestrictions restrictions =
    ul [ class "list2 custom-scrollbar" ] (ul [ ariaLabel "Restrições", class "list-title" ] [ div [ class "gg-add", onClick GoToAddRestrictionPage ] [] ] :: List.map renderRestriction restrictions)


renderRestriction : ( RestrictionID, Restriction ) -> Html Msg
renderRestriction ( id, restriction ) =
    li [ class "list-item", onClick (GoToEditRestrictionPage id) ]
        [ div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| convertWeekDay <| Just restriction.start_time ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| (++) "\t" <| (convertWeekTimeHourAndMinute <| Just restriction.start_time) ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| (++) "\t" <| convertWeekTimeHourAndMinute <| Just restriction.end_time ]
        , div [ class "custom-scrollbar", class "list-text", style "width" "10%" ] [ text <| (++) "\t" <| Restriction.categoryToPortugueseString restriction.category ]
        ]



------------------------ HTTP ------------------------


updateLect : ( LecturerID, Lecturer ) -> IsHidden -> String -> Token -> Cmd Msg
updateLect ( id, lect ) isHidden backendUrl token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers\\" ++ String.fromInt id
        , body = Http.jsonBody (Encoders.putLecturer Nothing lect isHidden)
        , expect = Http.expectJson UpdateLectResult (Decoders.responseParser Decoders.getLectAndID)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteLect : LecturerID -> String -> Token -> Cmd Msg
deleteLect id backendUrl token =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token), Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "lecturers\\" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever handleDeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleDeleteResponse : Result Http.Error () -> Msg
handleDeleteResponse response =
    case response of
        Ok _ ->
            DeleteLectResult (Ok ())

        Err err ->
            DeleteLectResult (Err err)
