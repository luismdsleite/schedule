module Pages.EditLect.Id_ exposing (Model, Msg, page)

import Decoders
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
import ScheduleObjects.Hide exposing (IsHidden)
import ScheduleObjects.Lecturer exposing (Lecturer, LecturerID, asLectIn, setLect, setLectAbbr, setLectName, setLectOffice)
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


type alias Model =
    { lectID : LecturerID
    , lect : Lecturer
    , restrictions : Dict RestrictionID Restriction
    , backendUrl : String
    , token : Token
    , deleteConfirmation : Bool
    , errorMsg : String
    , isHidden : Bool
    }


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
    ( Model lectID lect (Dict.filter (\_ restriction -> restriction.lect == lectID) data.restrictions) data.backendUrl data.token False "" isHidden, Effect.none )



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
update msg model =
    --  (Model ( lectID, lect ) restrictions backendUrl token deleteConfirmation errorMsg isHidden) =
    case msg of
        AbbrChange str ->
            ( setLectAbbr str model.lect |> asLectIn model, Effect.none )

        NameChange str ->
            ( setLectName str model.lect |> asLectIn model, Effect.none )

        OfficeChange str ->
            ( setLectOffice str model.lect |> asLectIn model, Effect.none )

        UpdateLectRequest ->
            ( model, Effect.sendCmd <| updateLect ( model.lectID, model.lect ) model.isHidden model.backendUrl model.token )

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
                    ( model, Effect.updateLect ( id, updatedLect ) (Just route) )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        DeleteLectRequest ->
            if model.deleteConfirmation then
                ( { model | deleteConfirmation = False }, Effect.sendCmd <| deleteLect model.lectID model.backendUrl model.token )

            else
                ( { model | deleteConfirmation = True }, Effect.none )

        DeleteLectResult result ->
            case result of
                Ok _ ->
                    ( model, Effect.deleteLect model.lectID (Just { path = Route.Path.Main, query = Dict.empty, hash = Nothing }) )

                Err (Http.BadStatus 500) ->
                    ( { model | errorMsg = "Erro no servidor. Verifique se ainda existem cadeiras (incluindo cadeiras escondidas) ou restrições associadas a este docente." }, Effect.none )

                Err err ->
                    ( { model | errorMsg = Decoders.errorToString err }, Effect.none )

        GoToEditRestrictionPage id ->
            ( model, Effect.pushRoute { path = Route.Path.EditRestriction_Id_ { id = String.fromInt id }, query = Dict.empty, hash = Nothing } )

        GoToAddRestrictionPage ->
            ( model, Effect.pushRoute { path = Route.Path.AddRestriction, query = Dict.singleton "lectID" (String.fromInt model.lectID), hash = Nothing } )

        VisibilityChange newVisibility ->
            ( { model | isHidden = newVisibility }, Effect.none )

        Return ->
            ( model, Effect.pushRoute { path = Route.Path.Main, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        orderedRestrictions =
            model.restrictions
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
        [ input [ class "input-box", style "width" "100%", value model.lect.abbr, onInput AbbrChange, Html.Attributes.placeholder "Abbreviatura" ] []
        , input [ class "input-box", style "width" "100%", value model.lect.name, onInput NameChange, Html.Attributes.placeholder "Nome Do Docente" ] []
        , input [ class "input-box", style "width" "100%", value model.lect.office, onInput OfficeChange, Html.Attributes.placeholder "Escritorio" ] []
        , renderRestrictions orderedRestrictions
        , div [] [ input [ type_ "checkbox", checked model.isHidden, onCheck VisibilityChange ] [], label [] [ text "Esconder Docente" ] ]
        , button [ style "margin-right" "2%", class "button", onClick Return ] [ text "Retornar" ]
        , button [ class "button", onClick UpdateLectRequest ] [ text "Submeter" ]
        , button [ style "margin-left" "2%", style "color" "red", class "button", onClick DeleteLectRequest ]
            [ text
                (if model.deleteConfirmation then
                    "Tem a certeza?"

                 else
                    "Eliminar"
                )
            ]
        , div [ style "width" "100%" ] [ text model.errorMsg ]
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
