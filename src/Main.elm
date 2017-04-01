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

import RouteUrl exposing (program, UrlChange)
import Navigation exposing (Location)
import RouteUrl.Builder as Builder

import Material

import Json.Decode as Json
import Dict

import Result.Extra as Result
import Task
import Time exposing (Time)

import ModelAndMsg exposing (initialModel, Model, Page (..), Msg (..))
import View exposing (view)
import Update exposing (update)

import App.View as App
import App.Update as App
import RedditAPI.Update as RedditAPI



-- Navigation


delta2url : Model -> Model -> Maybe UrlChange
delta2url oldModel newModel =
    let
        pageNumber =
            toString << .page << .app

        filters =
            .filters << .app

        username =
            .username << .session

        diff f =
            f oldModel /= f newModel

        history =
            if diff pageNumber || diff .currentPage ||
                (diff filters &&
                    (List.isEmpty (filters oldModel) || List.isEmpty (filters newModel)))
            then Builder.newEntry
            else Builder.modifyEntry

        myBuilder = Builder.builder
          |> history
          |> Builder.replacePath [String.toLower <| toString newModel.currentPage]
          |> Builder.insertQuery "page" (pageNumber newModel)
          |> (if not (diff filters) then identity else Builder.insertQuery "filters" (toString <| filters newModel))
          |> Builder.insertQuery "username" (Maybe.withDefault "" <| username newModel)
    in
      if diff .currentPage || diff pageNumber || diff filters || diff (.token << .session)
      then Just <| Builder.toHashChange myBuilder
      else Nothing


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
        setPage =
            if tokenMsg /= NoOp
            then SetPage App
            else SetPage page
    in
        Debug ("Hash = " ++ location.hash ++ " corrected to " ++ hash) :: setPage :: tokenMsg :: subMsgs



-- Main


init =
    ( initialModel, Task.perform SetNow Time.now )


main =
    program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = \model ->
            Sub.batch [ Time.every (30 * Time.second) SetNow
                      , Material.subscriptions Mdl model
                      , Sub.map AppMsg <| Material.subscriptions (App.Mdl) model.app
                      ]
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
        case query of
            "access_token" ->
                Result.andThen setToken <|
                    Json.decodeString Json.string ("\"" ++ value ++ "\"")

            "expires_in" ->
                Result.andThen setExpire <|
                    Json.decodeString Json.float value

            _ ->
                Err "Invalid query"
