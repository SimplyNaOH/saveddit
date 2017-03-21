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

module Update exposing (update)

import Material

import ModelAndMsg exposing (Msg (..), Model, Page (..))
import App.Update as App
import RedditAPI.Types as RedditAPI
import RedditAPI.Update as RedditAPI
import RedditAPI.Requests as RedditAPI

import Task

import Debug exposing (log)

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage newPage ->
            ( { model | currentPage =
                  -- provisory: do not allow going back to landing if we have a token
                  if newPage == Landing && model.session.token /= Nothing
                  then model.currentPage
                  else newPage }, Cmd.none )

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

        Mdl msg_ ->
          Material.update Mdl msg_ model

        Debug str ->
            ( { model | debug = model.debug ++ [ str ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
