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

module ModelAndMsg exposing (Page (..), Model, initialModel, Msg (..))


import Material

import Http

import App.Model as App
import App.Update as App
import RedditAPI.Types as RedditAPI
import RedditAPI.Session as RedditAPI
import RedditAPI.Update as RedditAPI

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
    , mdl : Material.Model
    , acceptedTOS : Bool
    , debug : List String
    }


initialModel =
    { currentPage = Landing, app = App.initialModel, session = RedditAPI.emptySession, now = 0, waitingForNow = [], loadedData = False, mdl = Material.model, acceptedTOS = False, debug = [] }

type Msg
    = SetPage Page
    | AppMsg App.Msg
    | APIMsg (RedditAPI.Msg Msg)
    | SetNow Time
    | DataResponse (Result Http.Error RedditAPI.RedditResponse)
    | Mdl (Material.Msg Msg)
    | Debug String
    | ToggleTOS
    | NoOp
