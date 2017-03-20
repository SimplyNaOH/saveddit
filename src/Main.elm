{-
   Copyright (C) 2017  SimplyNaOH

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- | This is the main module of the app. Here we route the SPA. Since at this
   stage this will be deployed to Github Pages, and we can't control the server,
   we will use Hash navigation for pages, and queries for options within a page.
-}


module Main exposing (..)

import Debug exposing (log)
import RouteUrl exposing (program, UrlChange)
import Navigation exposing (Location)
import UrlParser as Url
import UrlParser exposing ((</>), (<?>))
import RouteUrl.Builder as Builder


-- Html stuff

import Html exposing (div, text)
import Json.Decode as Json
import Dict
import Result.Extra as Result
import App.Model as App
import App.Update as App
import App.View as App
import RedditAPI.Session as RedditAPI
import RedditAPI.Update as RedditAPI
import RedditAPI.Requests as RedditAPI
import RedditAPI.Types as RedditAPI


-- ################## debug

import Task
import Http
import Process
import Time exposing (Time)


-- Model
-- | A page within the SPA


type Page
    = Landing
    | App
    | PrivacyPolicy
    | TermsOfUse
      -- not implemented yet
    | NotFound String



{- | In the model we keep track of the current page, and the app state/model. -}


type alias Model =
    { currentPage : Page
    , app : App.Model
    , session : RedditAPI.Session
    , now : Time
    , waitingForNow : List Msg
    , loadedData : Bool
    , debug : List String
    }


initialModel =
    { currentPage = Landing, app = App.initialModel, session = RedditAPI.emptySession, now = 0, waitingForNow = [], loadedData = False, debug = [] }



-- Update


type Msg
    = SetPage Page
    | AppMsg App.Msg
    | APIMsg (RedditAPI.Msg Msg)
    | SetNow Time
    | DataResponse (Result Http.Error RedditAPI.RedditResponse)
    | Debug String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage newPage ->
            ( { model | currentPage = newPage }, Cmd.none )

        AppMsg appMsg ->
            let
                ( updatedApp, appCmd ) =
                    App.update appMsg model.app
            in
                ( { model | app = updatedApp }, Cmd.map AppMsg appCmd )

        APIMsg apiMsg ->
            let
                ( updatedSession, sessionCmd ) =
                    RedditAPI.update model.now APIMsg apiMsg model.session

                savedRequest =
                    Maybe.withDefault Cmd.none <|
                        Maybe.map (Task.attempt DataResponse) <|
                            RedditAPI.savedRequest updatedSession ""

                newCmd =
                    case apiMsg of
                        RedditAPI.UsernameResponse _ ->
                            if not model.loadedData then
                                savedRequest
                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
                log ("Dispatching " ++ toString apiMsg ++ ". newCmd = " ++ toString newCmd) <|
                    if model.now /= 0 then
                        { model | session = updatedSession } ! [ sessionCmd, newCmd ]
                    else
                        ( { model | waitingForNow = model.waitingForNow ++ [ APIMsg apiMsg ] }, Cmd.none )

        SetNow now ->
            let
                chainUpdate msg ( model, cmd ) =
                    let
                        ( newModel, newCmd ) =
                            update msg model
                    in
                        newModel ! [ cmd, newCmd ]
            in
                List.foldl chainUpdate ( { model | now = now, waitingForNow = [] }, Cmd.none ) model.waitingForNow

        DataResponse (Ok response) ->
            let
                ( updatedApp, appCmd ) =
                    App.update (App.AddItems response.items) model.app

                savedRequest =
                    Maybe.withDefault Cmd.none <|
                        Maybe.map (Task.attempt DataResponse) <|
                            RedditAPI.savedRequest model.session response.after

                ( newModel, newCmd ) =
                    if List.length response.items == 100 then
                        ( model, savedRequest )
                    else
                        ( { model | loadedData = True }, Cmd.none )
            in
                -- TODO check (Dict.get "x-ratelimit-remaining" response.headers)
                { newModel | app = updatedApp, loadedData = True } ! [ Cmd.map AppMsg appCmd, newCmd ]

        DataResponse (Err error) ->
            update (Debug <| toString error) model

        Debug str ->
            ( { model | debug = model.debug ++ [ str ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- View


view model =
    div []
        [ Html.map AppMsg <| App.view model.app
        ]



-- Navigation


delta2url : Model -> Model -> Maybe UrlChange
delta2url oldModel newModel =
    let
        page =
            toString << .page << .app

        filters =
            .filters << .app

        username =
            .username << .session

        diff f =
            if f oldModel /= f newModel then
                Just
            else
                always Nothing

        maybePageUpdate : Maybe (Builder.Builder -> Builder.Builder)
        maybePageUpdate =
            diff page <|
                (\builder ->
                    builder
                        |> Builder.newEntry
                        |> Builder.insertQuery "page" (page newModel)
                )

        maybeUsernameUpdate =
            diff username <|
                (\builder ->
                    builder
                        |> Builder.modifyEntry
                        |> Builder.insertQuery "username" (Maybe.withDefault "" <| username newModel)
                )

        updates =
            [ maybePageUpdate, maybeUsernameUpdate ]

        isNothing maybe =
            case maybe of
                Nothing ->
                    True

                Just _ ->
                    False
    in
        if (List.isEmpty << List.filter (not << isNothing)) updates then
            Nothing
        else
            Just <| Builder.toHashChange << List.foldl (\f acc -> f acc) Builder.builder << List.map (Maybe.withDefault identity) <| updates


location2messages : Location -> List Msg
location2messages location =
    let
        -- | This is necessary because reddit redirects to 'website.com/#access_token...'
        hash =
            if String.startsWith "#!/" location.hash then
                location.hash
            else
                "#!/?" ++ String.dropLeft 1 location.hash

        page =
            (Maybe.withDefault Landing
                << Maybe.map (stringToPage)
                << List.head
                << Builder.path
                << Builder.fromHash
            )
                hash

        queries =
            Builder.query << Builder.fromHash <| hash

        toMsgResults =
            Dict.foldl (\q v l -> queryToMsg q v :: l) [] queries

        subMsgs =
            List.filter ((/=) NoOp)
                << List.map (Result.merge << Result.mapError (always NoOp))
            <|
                toMsgResults

        tokenQueries =
            Dict.filter (\query value -> query == "access_token" || query == "expires_in") queries

        tokenMsg =
            Result.merge
                << Result.mapBoth (always NoOp) (APIMsg << RedditAPI.SetToken)
            <|
                Result.andThen
                    (\res ->
                        if res.token == "" || res.expire == 0 then
                            Err "Failed to obtain queries"
                        else
                            Ok res
                    )
                <|
                    Dict.foldl tokenQueriesToToken (Ok { token = "", expire = 0 }) tokenQueries
    in
        Debug ("Hash = " ++ location.hash ++ " corrected to " ++ hash) :: SetPage page :: tokenMsg :: subMsgs



-- Main


init =
    ( initialModel, Task.perform SetNow Time.now )


main =
    program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = always <| Time.every (30 * Time.second) SetNow
        , view = view
        }



-- Helper


stringToPage str =
    case str of
        "app" ->
            App

        "privacy-policy" ->
            PrivacyPolicy

        "terms-of-use" ->
            TermsOfUse

        _ ->
            NotFound str


queryToMsg query value =
    case query of
        "page" ->
            Result.map (AppMsg << App.SetPage) (Json.decodeString Json.int value)

        "filtering" ->
            Result.map (AppMsg << App.SetFilters) (Json.decodeString (Json.list Json.string) value)

        _ ->
            Err "Invalid query"


tokenQueriesToToken query value token =
    let
        setToken newToken =
            Result.map (\token -> { token | token = newToken }) token

        setExpire newExpire =
            Result.map (\token -> { token | expire = newExpire }) token
    in
        log ("processing query " ++ query ++ " for token") <|
            case query of
                "access_token" ->
                    Result.andThen setToken <|
                        Json.decodeString Json.string ("\"" ++ value ++ "\"")

                "expires_in" ->
                    Result.andThen setExpire <|
                        Json.decodeString Json.float value

                _ ->
                    Err "Invalid query"
