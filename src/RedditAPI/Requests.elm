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

module RedditAPI.Requests exposing (..)

import Http
import Json.Decode as Json
import Json.Decode
    exposing
        ( map
        , map4
        , map7
        , map8
        , oneOf
        , andThen
        , succeed
        , fail
        , at
        , field
        , string
        , bool
        , int
        , list
        , decodeString
        )
import Time exposing (Time)
import Dict exposing (Dict)


-- For the helper retriedRequest

import Task exposing (Task)
import Process
import RedditAPI.Session exposing (Session)
import RedditAPI.Types exposing (..)


-- Commands and requests
-- Decoders


decodeSelf =
    field "data" <|
        map7 CommentInfo
            (field "id" string)
            (field "title" string)
            (field "url" string)
            (field "url" string)
            (field "selftext" string)
            (field "subreddit" string)
            (field "over_18" bool)


decodeComment =
    field "data" <|
        map7 CommentInfo
            (field "id" string)
            (field "link_title" string)
            (map4
                (\a b c d ->
                    "https://www.reddit.com/r/"
                        ++ a
                        ++ "/comments/"
                        ++ (String.dropLeft 3 b)
                        ++ "/"
                        ++ c
                        ++ "/"
                        ++ d
                        ++ "/"
                )
                (field "subreddit" string)
                (field "link_id" string)
                (field "link_title" string)
                (field "id" string)
            )
            (field "link_url" string)
            (field "body" string)
            (field "subreddit" string)
            (field "over_18" bool)


decodeThumbnail thumbnail =
    if String.startsWith "https://" thumbnail then
        succeed thumbnail
    else
        case thumbnail of
            "self" ->
                succeed "resources/self.png"

            _ ->
                oneOf [ at [ "preview", "images", "0", "resolutions", "0", "url" ] string, succeed "resources/noimage.png" ]


decodeLink =
    field "data" <|
        map8 LinkInfo
            (field "id" string)
            (field "title" string)
            (field "url" string)
            (map ((++) "https://www.reddit.com") (field "permalink" string))
            (field "thumbnail" string |> andThen decodeThumbnail)
            {- (oneOf
                [ at ["preview", "images", "0", "source", "url"] string
                , field "thumbnail" string]
               )
            -}
            (field "subreddit" string)
            (field "num_comments" int)
            (field "over_18" bool)


decodeItem =
    field "kind" string
        |> andThen
            (\kind ->
                case kind of
                    "t1" ->
                        map Comment decodeComment

                    "t3" ->
                        at [ "data", "is_self" ] bool
                            |> andThen
                                (\isSelf ->
                                    if isSelf then
                                        map Comment decodeSelf
                                    else
                                        map Link decodeLink
                                )

                    _ ->
                        fail "kind is not t1 or t3"
            )


decodeItems =
    at [ "data", "children" ] (list decodeItem)



-- TODO: Handle error here?


htmlResponseToRedditResponse response =
    Ok
        { headers = response.headers
        , items = Result.withDefault [] <| decodeString decodeItems response.body
        , after = Result.withDefault "" <| decodeString (at [ "data", "after" ] string) response.body
        }


usernameRequest : Session -> Maybe (Task Http.Error String)
usernameRequest session =
    Maybe.map
        ((\token ->
            retriedRequest "" 1 5 <|
                Http.request
                    { method = "GET"
                    , headers = [ Http.header "Authorization" ("bearer " ++ token) ]
                    , url = "https://oauth.reddit.com/api/v1/me"
                    , body = Http.emptyBody
                    , expect = Http.expectJson (field "name" string)
                    , timeout = Nothing
                    , withCredentials = False
                    }
         )
            << .token
        )
        session.token


emptyResponse =
    { headers = Dict.empty, items = [], after = "" }


savedRequest : Session -> String -> Maybe (Task Http.Error RedditResponse)
savedRequest session after =
    Maybe.map2
        (\token username ->
            retriedRequest emptyResponse 1 5 <|
                Http.request
                    { method = "GET"
                    , headers =
                        [ Http.header "Authorization" ("bearer " ++ token.token)
                        ]
                    , url =
                        "https://oauth.reddit.com/user/"
                            ++ username
                            ++ "/saved.json?raw_json=1&limit=100"
                            ++ if after /= "" then
                                "&after=" ++ after
                               else
                                ""
                    , body = Http.emptyBody
                    , expect = Http.expectStringResponse htmlResponseToRedditResponse
                    , timeout = Nothing
                    , withCredentials = False
                    }
        )
        session.token
        session.username



-- Helper


retriedRequest : a -> Time -> Int -> Http.Request a -> Task Http.Error a
retriedRequest pure delay numRetries request =
    let
        retry : Int -> Http.Error -> Task.Task Http.Error a
        retry n err =
            if n < numRetries then
                Task.onError (retry (n + 1)) <| Task.map (Maybe.withDefault pure << Maybe.andThen List.head << List.tail) <| Task.sequence [ Task.map (always pure) <| Process.sleep delay, Http.toTask request ]
            else
                Task.fail err
    in
        Task.onError (retry 1) <| Http.toTask request
