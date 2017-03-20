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

module App.Model exposing (..)

import RedditAPI.Types exposing (Item, Subreddit, subreddit, isNSFW)
import SearchableMenu as Menu


-- Model
{- | Is a subreddit NSFW? At this time, we don't check if a subreddit is NSFW
   but rather wether the posts we have from a particular subreddit are NSFW. If all
   the posts we have from a subreddit are NSFW, the sub is marked as NSFW, if some
   of them are it is marked as Partial, else it is marked as SFW.
-}


type SubOver18
    = SFW
    | Partial
    | NSFW



{- | We keep associate data for a subreddit, beyond its name. Namely, how many
   posts belonging to it we have and wether they are NSFW.
-}


type alias SubredditInfo =
    { subreddit : Subreddit
    , numberOfItems : Int
    , over18 : SubOver18
    }



{- | The model contains the items (posts), all the subreddits they belong to,
   the subreddits we are currently filtering by, wether the user wants to be shown
   NSFW posts and subreddits, where the current page starts and how many items to
   show per page, and finaly, the state of the subreddit selection menu.
-}


type alias Model =
    { items : List Item
    , subreddits : List SubredditInfo
    , filters : List Subreddit
    , showNSFW : Bool
    , page : Int
    , sliceLength : Int
    , menu : Menu.Model
    }


initialModel =
    Model [] [] [] False 0 15 Menu.initialModel



-- Helpers
{- | Filter the items based on active subreddit filters and wether we are
   showing NSFW posts.
-}


filtered model =
    let
        isInFilteredSub item =
            if List.isEmpty model.filters then
                True
            else
                List.member (subreddit item) model.filters

        checkNSFW item =
            not <| not model.showNSFW && (isNSFW item)
    in
        List.filter (\item -> isInFilteredSub item && checkNSFW item) model.items
