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

module View exposing (view)

-- Html stuff

import Html exposing (div, text, span)
import Html.Attributes exposing (class)

import Material
import Material.Layout as Layout
import Material.Button as Button

import ModelAndMsg exposing (Msg (..))
import App.View as App


-- View


header model =
  Layout.row []
    [ Layout.title [] [text "saveddit"]
    , text <| toString model.currentPage
    , Layout.spacer
    , Layout.navigation []
      [ Button.render Mdl [0] model.mdl
        [ Button.icon
        , Button.link "https://github.com/simplynaoh/saveddit"
        ]
        [ span [class "fa fa-github"] []]
      ]
    ]

view model =
  Layout.render Mdl model.mdl
    [ Layout.fixedHeader
    ]
    { header = [ header model ]
    , drawer = [text "options"]
    , tabs = ([], [])
    , main = [ div []
        [ Html.map AppMsg <| App.view model.app
        ] ]
    }
