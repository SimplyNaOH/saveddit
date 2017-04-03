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

module App.ItemListView exposing (..)

import Material.List as Lists
import Material.Options as Options
import Material.Button as Button
import Material.Icon as Icon
import Material.Color as Color

import Html exposing (div, ul, li, span, a, h1, p, text, img, i)
import Html.Attributes exposing (class, href, target, src, attribute, id)
import Html.Keyed as Keyed
import RedditAPI.Types exposing (Item (..), Subreddit)
import App.Model exposing (filtered)
import App.Update exposing (Msg (..))


-- View

itemsView model =
  let
    itemView idx item =
      case item of
        Link info ->
          Lists.li []
              [ Lists.content []
                  [ a [href info.url, target "_blank"]
                      [ Lists.avatarImage info.previewURL []
                      , span [] [text info.title]
                      ]
                  ]
              , Lists.content2 []
                [ Lists.info2 [] [ text <| toString info.numComments ++ " ", i [ class "fa fa-comments", attribute "aria-hidden" "true" ] [] ]
                , Options.span [Lists.action2]
                  [ Button.render Mdl [1, idx] model.mdl
                    [ Button.icon
                    , Button.colored
                    , Button.link info.commentsURL
                    , Options.attribute <| Html.Attributes.target "_blank"
                    ]
                    [ Icon.i "comment" ]
                  ]
                , Options.span [Lists.action2]
                    [ Button.render Mdl [0, idx] model.mdl
                        [ Button.icon
                        , Button.colored
                        , Button.link info.url
                        , Options.attribute <| Html.Attributes.target "_blank"
                        ]
                        [ Icon.i "open_in_new" ]
                    ]
                ]
              ]
        Comment info ->
          Lists.li [ Lists.withBody, Options.cs "comment" ] -- NB! Required on every Lists.li containing body.
              [ Lists.content []
                  [ Lists.avatarIcon "comment" [Color.background Color.accent]
                  , text info.title
                  , Lists.body [] [ a [href info.url, target "_blank"] [ text info.body ] ]
                  ]
              , Button.render Mdl [0, idx] model.mdl
                  [ Button.icon
                  , Button.colored
                  , Button.link info.url
                  , Options.attribute <| Html.Attributes.target "_blank"
                  ]
                  [ Icon.i "open_in_new" ]
              ]


  in
    Lists.ul [] <|
        List.indexedMap itemView <|
            List.take model.sliceLength <|
                List.drop (model.page * model.sliceLength) <|
                    filtered model


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
