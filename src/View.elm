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

import Html exposing (div, h1, p, text, span, img, a)
import Html.Attributes exposing (class, style, href, target)

import Svg
import Svg.Attributes as Svg


import Material
import Material.Layout as Layout
import Material.Button as Button

import ModelAndMsg exposing (Msg (..), Page (..))
import App.View as App
import RedditAPI.Requests as RedditAPI

-- View
{-
logo =
  Svg.svg [Svg.width "133", Svg.height "31"]
    [Svg.text_ [Svg.x "0", Svg.y "31", Svg.fontSize "31px", Svg.fontFamily "sans-serif"]
      [ Svg.tspan [Svg.fill "#00b358", Svg.dx "0 -4 -4 -4 -4"] [Svg.text "saved"]
      , Svg.tspan [Svg.fill "#67da00", Svg.fillOpacity "0.5", Svg.dx "-4"] [Svg.text "d"]
      , Svg.tspan [Svg.fill "#ff8c00", Svg.dx "-4 -4"] [Svg.text "it"]
      ]
    ]
-}
logo = img [Html.Attributes.src "logo.svg"] []

notFoundView str = [ Html.p [] [text "Error 404"]
                   , Html.p [] [text (str ++ " not found.")]
                   ]

landingView model =
  [div [class "landing"]
    [ div [class "landing__logo"]
          [ logo
          , p [] [text "Filter your saved posts"]
          ]
    , Html.h3 [] [text "What?"]
    , p [] [text "This webapp allows you to view your reddit saved posts organized by subreddit. It's an experiment on using Elm and thus not a serious project, use it under your own risk!"]
    , Html.h3 [] [text "How?"]
    , p [] [ text "In order to show you your saved posts you need to give the app permission to access your reddit account. Dont't worry! This app is 100% client-side and "
           , a [href "https://github.com/simplynaoh/saveddit", target "_blank"] [text "open source"]
           , text ", which means that you can rest assured that your data stays with you. Moreover, this app will only ask you for access to your username and reddit history."
           ]
    , p [] [Html.b [] [text "Having said that, your data (access token, username and saved posts) will remain in your device's memory for a short period of time. If your device is compromised, we can't guarantee the integrity and safety of your data."]]
    , p [] [text "When you are ready, click the following button to ask reddit for permission into your account (don't worry, reddit will ask for confirmation!):"]
    , div [class "landing__login-button" ]
        [ Button.render Mdl [0] model.mdl
            [ Button.raised
            , Button.accent
            , Button.ripple
            , Button.link RedditAPI.accessRequestUrl
            ]
            [ text "Loggin with reddit"]
        ]
    ]
  ]


appView model = [Html.map AppMsg <| App.view model.app]

pageView model =
  case model.currentPage of
    Landing ->
      landingView model
    App ->
      appView model
    PrivacyPolicy ->
      notFoundView "PrivacyPolicy"
    TermsOfUse ->
      notFoundView "TermsOfUse"
    NotFound str ->
      notFoundView str



header model =
  Layout.row []
    [ Layout.title [] [text "saveddit"]
    , Html.span [] [text <| toString model.currentPage]
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
    , drawer = [logo, p [] [text "options"]]
    , tabs = ([], [])
    , main = pageView model
    }
