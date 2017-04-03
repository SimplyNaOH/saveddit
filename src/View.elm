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
import Material.Toggles as Toggles
import Material.Options as Options

import ModelAndMsg exposing (Msg (..), Page (..))
import App.View as App
import RedditAPI.Requests as RedditAPI

import Pages.PrivacyPolicy as PrivacyPolicy
import Pages.TermsOfUse as TermsOfUse

-- View

logo = img [Html.Attributes.src "resources/logo.svg"] []
altlogo = img [Html.Attributes.src "resources/alt-logo.svg", style [("height", "20px"), ("padding-bottom", "4px")]] []

notFoundView str = [ Html.p [] [text "Error 404"]
                   , Html.p [] [text (str ++ " not found.")]
                   ]

landingView model =
  let
    login = div [class "landing__login" ]
        [ Button.render Mdl [0] model.mdl
            [ Button.raised
            , Button.accent
            , Button.ripple
            , Button.disabled |> Options.when (not model.acceptedTOS)
            , Button.link RedditAPI.accessRequestUrl |>
                Options.when model.acceptedTOS
            , Options.cs "landing__login-button"
            ]
            [ text "Login with reddit"]
        , Toggles.checkbox Mdl [0] model.mdl
          [ Options.onToggle ToggleTOS
          , Toggles.ripple
          , Toggles.value model.acceptedTOS
          , Options.cs "landing__login-checkbox"
          ]
          [ text "I have read and agree with the "
          , a [href "#!/terms-of-use"] [text "Terms of Use"]
          , text " and the "
          , a [href "#!/privacy-policy"] [text "Privacy Policy"]
          , text "." ]
        ]
  in
  [div [class "landing"]
    [ div [class "landing__logo"]
          [ logo
          , p [] [text "Filter your saved posts"]
          ]
    , login
    , Html.h3 [] [text "What?"]
    , p [] [text "This webapp allows you to ", Html.b [class "mdl-color-text--primary"] [text "view your reddit saved posts organized by subreddit"], text ". It is an ongoing open source project written in Elm. Suggestions and contributions are welcome, head over to the ", a [href "https://github.com/simplynaoh/saveddit", target "_blank"] [text "github repo"], text "!"]
    , Html.h3 [] [text "How?"]
    , p [] [ text "In order to show you your saved posts you need to give the app permission to access your reddit account. Dont't worry! This app is 100% client-side and open source, which means that you can rest assured that your data stays with you. Moreover, this app will only ask you for access to your username and reddit history."]
    , p [] [Html.b [] [text "Having said that, your data (access token, username and saved posts) will remain in your device's memory for a short period of time. If your device is compromised, we can't guarantee the integrity and safety of this information."]]
    , p [] [text "When you are ready, click the following button to ask reddit for permission into your account; reddit will ask for confirmation:"]
    , login
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
      PrivacyPolicy.view
    TermsOfUse ->
      TermsOfUse.view
    NotFound str ->
      notFoundView str



header model =
  Layout.row []
    [ Layout.title [] [altlogo]--[text "saveddit"]
    --, Html.span [] [text <| toString model.currentPage]
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
