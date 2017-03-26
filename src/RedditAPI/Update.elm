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

module RedditAPI.Update exposing (..)

import RedditAPI.Session exposing (Session, Token)
import RedditAPI.Requests as Requests
import RedditAPI.Types exposing (RedditResponse)
import Http
import Task
import Dict
import Time exposing (Time)


type Msg msg
    = SetToken Token
    | UsernameResponse (Result Http.Error String)
    | RequestSaved (Result Http.Error RedditResponse -> msg) String


update : Time -> (Msg msg -> msg) -> Msg msg -> Session -> ( Session, Cmd msg )
update now wrapMsg msg session =
    case msg of
        SetToken newTokenAndExpire ->
            let
                newToken =
                    newTokenAndExpire.token

                newExpire =
                    newTokenAndExpire.expire + now

                withNewToken =
                    { session | token = Just <| Token newToken newExpire }

                newSession =
                    case session.token of
                        Just { expire } ->
                            if expire < newExpire then
                                withNewToken
                            else
                                session

                        Nothing ->
                            withNewToken

                usernameRequest =
                    Maybe.withDefault (Task.succeed "This should never happen!") <|
                        Requests.usernameRequest newSession
            in
                ( newSession, Task.attempt (wrapMsg << UsernameResponse) usernameRequest )

        UsernameResponse (Ok username) ->
            ( { session | username = Just username }, Cmd.none )

        UsernameResponse (Err error) ->
            ( { session | username = Just <| toString error }, Cmd.none )

        RequestSaved msg after ->
            let
                usernameRequest =
                    Maybe.withDefault (Task.succeed "This should never happen!") <|
                        Requests.usernameRequest session

                savedRequest =
                    Maybe.withDefault (Task.succeed { headers = Dict.empty, items = [], after = "This should never happen!" }) <|
                        Requests.savedRequest session after
            in
                case ( session.token, session.username ) of
                    ( Just token, Just username ) ->
                        ( session, Task.attempt msg savedRequest )

                    ( Just token, Nothing ) ->
                        ( session, Task.attempt (wrapMsg << UsernameResponse) usernameRequest )

                    _ ->
                        ( session, Cmd.none )
