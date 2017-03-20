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

module RedditAPI.Types exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)


-- Data types


type alias Token =
    { token : String, expire : Time }



{- | Record to store data of link posts -}


type alias LinkInfo =
    { id : String
    , title : String
    , url : String
    , commentsURL : String
    , previewURL : String
    , subreddit : String
    , numComments : Int
    , over18 : Bool
    }



{- | Record to store data of comment and self posts. -}


type alias CommentInfo =
    { id : String
    , title : String
    , url : String
    , linkURL : String
    , body : String
    , subreddit : String
    , over18 : Bool
    }



{- | Any item we want to display is stored either of the previous records. -}


type Item
    = Link LinkInfo
    | Comment CommentInfo


type alias RedditResponse =
    { headers : Dict String String
    , items : List Item
    , after : String
    }



{- | An actual subreddit is just a string. -}


type alias Subreddit =
    String



-- Helper functions for items:


isNSFW : Item -> Bool
isNSFW item =
    case item of
        Link link ->
            link.over18

        Comment comment ->
            comment.over18


subreddit : Item -> Subreddit
subreddit item =
    case item of
        Link link ->
            link.subreddit

        Comment comment ->
            comment.subreddit
